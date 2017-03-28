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
case class TransferId(id: UUID) extends Id
case class EntryId(id: UUID) extends Id
case class AccountId(id: UUID) extends Id
case class CurrencyId(id: UUID) extends Id

sealed trait NamedObject {
  def name: String
}
sealed trait EntityKind

case class User(id: UserId, name: String) extends NamedObject

sealed trait Category extends NamedObject {
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

  case class SharedData(id: CategoryId, name: String)

  case class Root(
    data: SharedData, kind: Kind
  ) extends Category
  case class Child(
    data: SharedData, parentId: CategoryId
  ) extends Category
}

case class Tag(id: TagId, name: String) extends NamedObject
object Tag extends EntityKind

case class Account(
  id: AccountId, name: String,
  initialBalance: Money, initialBalanceDate: Date,
  currency: CurrencyId
) extends NamedObject
object Account extends EntityKind

case class Currency(
  id: CurrencyId, name: String, shortName: String
) extends NamedObject
object Currency extends EntityKind

case class Transfer(
  id: TransferId, from: AccountId, to: AccountId, sum: Money
)

case class Entry(
  id: EntryId, account: AccountId, category: CategoryId,
  name: String, sum: Money
) extends NamedObject

case class Database(
  owner: User,
  accounts: Map[AccountId, Account],
  currencies: Map[CurrencyId, Currency],
  categories: Map[CategoryId, Category],
  tags: Map[TagId, Tag],
  entries: Vector[Entry],
  transfers: Vector[Transfer]
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

  def empty(owner: User): Database = apply(
    owner, Map.empty, Map.empty, Map.empty, Map.empty, Vector.empty, Vector.empty
  )

  def xOrCreate[A](
    get: Database => Option[A],
    create: () => State[Database, A]
  ): State[Database, A] =
    State { db =>
      get(db).fold(create().run(db).value)((db, _))
    }

  def createCurrency(
    name: String, shortName: String
  ): State[Database, Currency] =
    State { db =>
      val currency = Currency(CurrencyId(UUID.randomUUID()), name, shortName)
      (
        db.modify(_.currencies.at(currency.id)).setTo(currency),
        currency
      )
    }

  def createCategory(
    name: String, kind: Category.Kind
  ): State[Database, Category] =
    State { db =>
      val category = Category.Root(
        Category.SharedData(CategoryId(UUID.randomUUID()), name),
        kind
      )
      (db.modify(_.categories.at(category.id)).setTo(category), category)
    }

  def createAccount(
    name: String, currencyId: CurrencyId,
    initialBalance: Money, initialBalanceDate: Date
  ): State[Database, Account] =
    State { db =>
      val account = Account(
        AccountId(UUID.randomUUID()), name,
        initialBalance, initialBalanceDate, currencyId
      )
      (db.modify(_.accounts.at(account.id)).setTo(account), account)
    }

  def createEntry(
    name: String, accountId: AccountId, categoryId: CategoryId,
    sum: Money
  ): State[Database, Entry] =
    State { db =>
      val entry = Entry(EntryId(UUID.randomUUID()), accountId, categoryId, name, sum)
      (db.modify(_.entries).using(_ :+ entry), entry)
    }

  def createTransfer(
    fromAccount: AccountId, toAccount: AccountId, sum: Money
  ): State[Database, Transfer] =
    State { db =>
      val transfer = Transfer(TransferId(UUID.randomUUID()), fromAccount, toAccount, sum)
      (db.modify(_.transfers).using(_ :+ transfer), transfer)
    }
}