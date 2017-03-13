package net.arturaz.family_money_tracker.importer

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.{Date, UUID}

import cats.data.{State, StateT}
import kantan.codecs.Result
import kantan.csv
import kantan.csv.RowDecoder
import kantan.csv._
import kantan.csv.ops._
import net.arturaz.family_money_tracker.data._
import com.softwaremill.quicklens._

import scala.annotation.tailrec
import scala.util.Try

case class MonefyEntry(
  date: Date, account: String, category: String, amount: BigDecimal, currency: String,
  description: String
)
object MonefyEntry {
  implicit val decoder = RowDecoder.decoder(0, 1, 2, 3, 4, 7)(apply)

  object InitialBalance {
    def category(account: String): String = s"Initial balance '$account'"

    def unapply(entry: MonefyEntry): Option[MonefyEntry] = {
      if (entry.category == category(entry.account)) Some(entry)
      else None
    }
  }
}

object MonefyImporter {
  case class ImportDB(

  )

  sealed trait Error
  case class ReadError(throwable: Throwable) extends Error
  case class ParsingError(errors: List[csv.ReadError]) extends Error
  case class LogicError(error: Database.LookupError) extends Error

  def read(file: Path, owner: UserId, database: Database): Either[Error, Database] = {
    def currency(entry: MonefyEntry): State[Database, Currency] =
      Database.xOrCreate(
        _.currencyByName(entry.currency),
        () => Database.createCurrency(owner, entry.currency, entry.currency)
      )

    def createAccount(entry: MonefyEntry): State[Database, Account] = for {
      currency <- currency(entry)
      account <- Database.createAccount(
        owner, entry.account, currency.id, Money(entry.amount), entry.date
      )
    } yield account

    Try {
      new String(Files.readAllBytes(file), StandardCharsets.UTF_8)
    } match {
      case util.Failure(err) => Left(ReadError(err))
      case util.Success(content) =>
        val csvContents = content.asCsvReader[MonefyEntry](rfc.withHeader).toList

        val errors = csvContents.collect { case Result.Failure(err) => err }
        if (errors.nonEmpty) Left(ParsingError(errors))
        else {
          val successes = csvContents.collect { case Result.Success(entry) => entry }

          @tailrec def rec(
            database: Database, entries: List[MonefyEntry]
          ): Either[LogicError, Database] = entries match {
            case MonefyEntry.InitialBalance(entry) :: rest =>
              rec(createAccount(entry).runS(database).value, rest)

            // TODO: handle transfers

            case entry :: rest =>
              val either = for {
                account <- database.accountByNameE(entry.account).right
              } yield {
                val kind = Category.Kind.fromSign(entry.amount)
                val state = for {
                  category <- Database.xOrCreate[Category](
                    _.categoryBy { c =>
                      c.name == entry.category && database.kindOf(c.id).contains(kind)
                    },
                    () => Database.createCategory(owner, entry.category, kind)
                  )
                  entry <- Database.createEntry(
                    owner, entry.description, account.id, category.id, Money(entry.amount.abs)
                  )
                } yield entry

                state.runS(database).value
              }

              either match {
                case Left(err) => Left(LogicError(err))
                case Right(db) => rec(db, rest)
              }

            case Nil =>
              Right(database)
          }

          rec(database, successes)
        }
    }
  }
}