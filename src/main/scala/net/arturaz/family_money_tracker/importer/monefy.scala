package net.arturaz.family_money_tracker.importer

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.Date

import cats.Eval
import cats.data.{State, StateT}
import kantan.codecs.Result
import kantan.csv.RowDecoder
import kantan.csv._
import kantan.csv.ops._
import net.arturaz.family_money_tracker.data.{Account, Database, Money, UserId}

import scala.util.Try
import scala.util.matching.Regex

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
  def read(file: Path, owner: UserId) = Try {
    def account(entry: MonefyEntry): State[Database, Account] = for {
      currency <- Database.currencyFor(owner, entry.currency, entry.currency)
      account <- Database.accountFor(
        owner, entry.account, currency.id, Money(entry.amount), entry.date
      )
    } yield account

    val content = new String(Files.readAllBytes(file), StandardCharsets.UTF_8)
    val csvIterator = content.asCsvReader[MonefyEntry](rfc.withHeader)
    csvIterator.foldLeft(
      (Database.empty, Vector.empty[ReadError])
    ) {
      case ((db, errors), Result.Success(MonefyEntry.InitialBalance(entry))) =>
        (account(entry).runS(db).value, errors)
      case ((db, errors), Result.Success(entry)) =>
        (db, errors)
      case ((db, errors), Result.Failure(err)) =>
        (db, errors :+ err)
    }
  }
}