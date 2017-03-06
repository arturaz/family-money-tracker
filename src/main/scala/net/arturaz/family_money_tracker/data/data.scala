package net.arturaz.family_money_tracker.data

import java.util.{Date, UUID}

import cats.data.State
import com.softwaremill.quicklens._

sealed trait Id {
  val id: UUID
}
case class UserId(id: UUID) extends Id
case class CategoryId(id: UUID) extends Id
case class TagId(id: UUID) extends Id
case class EntryId(id: UUID) extends Id
case class AccountId(id: UUID) extends Id
case class CurrencyId(id: UUID) extends Id

sealed trait OwnedObject {
  def owner: UserId
}

case class User(id: UserId, alias: String)

sealed trait Category extends OwnedObject
object Category {
  sealed trait Kind
  case object Expense extends Kind
  case object Income extends Kind

  case class SharedData(id: CategoryId, name: String, owner: UserId)

  case class Root(
    data: SharedData, kind: Kind
  ) extends Category {
    override def owner = data.owner
  }
  case class Child(
    data: SharedData, parentId: CategoryId
  ) extends Category {
    override def owner = data.owner
  }
}

case class Tag(id: TagId, owner: UserId, name: String) extends OwnedObject

case class Account(
  id: AccountId, owner: UserId, name: String,
  initialBalance: Money, initialBalanceDate: Date,
  currency: CurrencyId
) extends OwnedObject

case class Currency(
  id: CurrencyId, owner: UserId, name: String, shortName: String
) extends OwnedObject

case class Entry(
  id: EntryId, account: AccountId, category: CategoryId, owner: UserId,
  name: String, sum: Money
) extends OwnedObject

case class Database(
  accounts: Map[AccountId, Account],
  currencies: Map[CurrencyId, Currency],
  categories: Map[CategoryId, Category],
  tags: Map[TagId, Tag],
  entries: Vector[Entry]
)
object Database {
  val empty = apply(
    Map.empty, Map.empty, Map.empty, Map.empty, Vector.empty
  )

  def currencyFor(
    owner: UserId, name: String, shortName: String
  ): State[Database, Currency] =
    State { db =>
      db.currencies.find(_._2.name == name) match {
        case Some((id, currency)) => (db, currency)
        case None =>
          val currency = Currency(CurrencyId(UUID.randomUUID()), owner, name, shortName)
          (
            db.modify(_.currencies.at(currency.id)).setTo(currency),
            currency
          )
      }
    }

  def accountFor(
    owner: UserId, name: String, currencyId: CurrencyId,
    initialBalance: Money, initialBalanceDate: Date
  ): State[Database, Account] =
    State { db =>
      db.accounts.find(_._2.name == name) match {
        case Some((id, account)) => (db, account)
        case None =>
          val account = Account(
            AccountId(UUID.randomUUID()), owner, name,
            initialBalance, initialBalanceDate, currencyId
          )
          (
            db.modify(_.accounts.at(account.id)).setTo(account),
            account
          )
      }
    }
}

