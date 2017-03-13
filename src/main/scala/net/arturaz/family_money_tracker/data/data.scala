package net.arturaz.family_money_tracker.data

import java.util.{Date, UUID}

import cats.data.State
import com.softwaremill.quicklens._
import net.arturaz.family_money_tracker.data.Database.LookupError

import scala.annotation.tailrec

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
sealed trait NamedObject {
  def name: String
}
sealed trait EntityKind

case class User(id: UserId, name: String) extends NamedObject

sealed trait Category extends OwnedObject with NamedObject {
  def data: Category.SharedData
  @inline def id: CategoryId = data.id
  @inline def name: String = data.name
}
object Category extends EntityKind {
  sealed trait Kind
  object Kind {
    def fromSign(bd: BigDecimal): Kind = fromBool(bd > 0)
    def fromBool(income: Boolean): Kind = if (income) Income else Expense

    case object Expense extends Kind
    case object Income extends Kind
  }

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

case class Tag(id: TagId, owner: UserId, name: String) extends OwnedObject with NamedObject
object Tag extends EntityKind

case class Account(
  id: AccountId, owner: UserId, name: String,
  initialBalance: Money, initialBalanceDate: Date,
  currency: CurrencyId
) extends OwnedObject with NamedObject
object Account extends EntityKind

case class Currency(
  id: CurrencyId, owner: UserId, name: String, shortName: String
) extends OwnedObject with NamedObject
object Currency extends EntityKind

case class Entry(
  id: EntryId, account: AccountId, category: CategoryId, owner: UserId,
  name: String, sum: Money
) extends OwnedObject with NamedObject

case class Database(
  accounts: Map[AccountId, Account],
  currencies: Map[CurrencyId, Currency],
  categories: Map[CategoryId, Category],
  tags: Map[TagId, Tag],
  entries: Vector[Entry]
) {
  def currencyByName(name: String): Option[Currency] =
    Database.findByName(currencies, name)
  def currencyByNameE(name: String): Either[Database.LookupError, Currency] =
    currencyByName(name).toRight(LookupError(Currency, name))

  def accountByName(name: String): Option[Account] =
    Database.findByName(accounts, name)
  def accountByNameE(name: String): Either[Database.LookupError, Account] =
    accountByName(name).toRight(LookupError(Account, name))

  def categoryBy(predicate: Category => Boolean): Option[Category] =
    Database.findBy(categories)(predicate)

  @tailrec final def kindOf(categoryId: CategoryId): Option[Category.Kind] =
    categories.get(categoryId) match {
      case Some(c: Category.Root) => Some(c.kind)
      case Some(c: Category.Child) => kindOf(c.parentId)
      case None => None
    }
}
object Database {
  case class LookupError(kind: EntityKind, name: String)

  def findByName[K, V <: NamedObject](map: Map[K, V], name: String): Option[V] =
    findBy(map)(_.name == name)

  def findBy[K, V](map: Map[K, V])(predicate: V => Boolean): Option[V] =
    map.find(t => predicate(t._2)).map(_._2)

  val empty = apply(
    Map.empty, Map.empty, Map.empty, Map.empty, Vector.empty
  )

  def xOrCreate[A](
    get: Database => Option[A],
    create: () => State[Database, A]
  ): State[Database, A] =
    State { db =>
      get(db).fold(create().run(db).value)((db, _))
    }

  def createCurrency(
    owner: UserId, name: String, shortName: String
  ): State[Database, Currency] =
    State { db =>
      val currency = Currency(CurrencyId(UUID.randomUUID()), owner, name, shortName)
      (
        db.modify(_.currencies.at(currency.id)).setTo(currency),
        currency
      )
    }

  def createCategory(
    owner: UserId, name: String, kind: Category.Kind
  ): State[Database, Category] =
    State { db =>
      val category = Category.Root(
        Category.SharedData(CategoryId(UUID.randomUUID()), name, owner),
        kind
      )
      (db.modify(_.categories.at(category.id)).setTo(category), category)
    }

  def createAccount(
    owner: UserId, name: String, currencyId: CurrencyId,
    initialBalance: Money, initialBalanceDate: Date
  ): State[Database, Account] =
    State { db =>
      val account = Account(
        AccountId(UUID.randomUUID()), owner, name,
        initialBalance, initialBalanceDate, currencyId
      )
      (db.modify(_.accounts.at(account.id)).setTo(account), account)
    }

  def createEntry(
    owner: UserId, name: String, accountId: AccountId, categoryId: CategoryId,
    sum: Money
  ): State[Database, Entry] =
    State { db =>
      val entry = Entry(EntryId(UUID.randomUUID()), accountId, categoryId, owner, name, sum)
      (db.modify(_.entries).using(_ :+ entry), entry)
    }
}

