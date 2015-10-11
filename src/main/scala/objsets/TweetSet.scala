package objsets

import common._
import TweetReader._

/**
 * Класс, представляющий твиты.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"

  def mentions(keywords: List[String]): Boolean = keywords.exists(x => text.contains(x))
}

/**
 * Представляет множество объектов типа`Tweet` в форме поиска в бинарном дереве.
 * Каждая ветка дерева имеет двух детей (два `TweetSet`а). Есть 
 * инвариант, который всегда выполняется: для каждой ветви `b`, все элементы поддерева слева
 * меньше, чем твит в `b`. Элементы в правом поддереве больше.
 *
 * Обратите внимание, что данная структура позволяет нам сравнивать два твита (нам
 * нужно уметь сказать какой из твитов больше, или они равны). В
 * данной реализации равенство / порядок твитов базируется на тексте твита
 * (см. `def incl`). `TweetSet` не может содержать два твита с одинаковым текстом 
 * от различных пользователей.
 *
 *
 * Преимущества представления множества в качестве бинарных деревьев в том, что элементы
 * этого множества можно быстро искать. Если вы хотите, вы можете узнать больше в Википедии[1], 
 * но это не обязательно для того чтобы выполнить это задание.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * ЭТот метод принимает предикат и возвращает подмножество всеъ элементов 
   * исходного множества для которого предикат истинен.
   *
   * Вопрос: Можем ли мы реализовать этот метод здесь, или следует его оставить abstract
   * и реализовать в подклассах?
   */
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)
  
  /**
   * Это вспомогательный метод для `filter` который передает аккумулированные твиты
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Возвращает новый `TweetSet` который является объединением `TweetSet`ов `this` и `that`.
   *
   * Вопрос: Можем ли мы реализовать этот метод здесь, или следует его оставить abstract
   * и реализовать в подклассах?
   */
  def union(that: TweetSet): TweetSet
  
  /**
   * Возвращает твит с наибольшим количествам ретвитов из множества.
   *
   * Вызов `mostRetweeted` на пустом множестве должен кинуть исключение
   * типа `java.util.NoSuchElementException`.
   *
   * Вопрос: Можем ли мы реализовать этот метод здесь, или следует его оставить abstract
   * и реализовать в подклассах?
   */
  def mostRetweeted: Tweet

  def isEmpty: Boolean
  
  /**
   * Возвращает список, содержащий все твиты этого множества, упорядоченные по количеству ретвитов
   * в убывающем порядке. Другими словами, голова результирующего списка должна
   * иметь максимальное количество ретвитов.
   *
   * Подсказка: Метод `remove` в TweetSet будет очень полезен.
   * Вопрос: Можем ли мы реализовать этот метод здесь, или следует его оставить abstract
   * и реализовать в подклассах?
   */
  def descendingByRetweet: TweetList = {
    def loop(in: TweetSet, ts: TweetList): TweetList = {
      if (in.isEmpty) ts
      else {
        val mostPopularTweet = in.mostRetweeted
        new Cons(mostPopularTweet, loop(in.remove(mostPopularTweet), ts))
      }
    }
    loop(this, Nil)
  }

  def mentions(keywords: List[String]): TweetSet = filter(x => x.mentions(keywords))
  
  /**
   * Следующие методы уже реализованы
   */

  /**
   * Вовзаращет новый `TweetSet` который содержит все элементы множества и 
   * и новый элемент `tweet` в случае если он уже не находится в исходном множестве.
   *
   * Если `this.contains(tweet)`, возвращается текущее множество.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Возвращает новый `TweetSet`, исключая `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Проверяет содержится ли`tweet` в данном `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * Этот метод принимает функцию и применяет ее к каждому элементу множества
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = new Empty

  def union(that: TweetSet): TweetSet = that

  def mostRetweeted: Tweet = throw new NoSuchElementException

  def isEmpty = true
  
  /**
   * Следующие методы уже реализованы
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

    def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
      val l = left.filterAcc(p, acc)
      val r = right.filterAcc(p, acc)
      val lr = l union r
      if (p(elem)) lr.incl(elem) else lr
    }

  def union(that: TweetSet): TweetSet = right.union( left.union( that.incl(elem) ) )

  def isEmpty = false

  def mostRetweeted: Tweet = {
    val all = right.union(left);
    val morePopular = all.filter(p => p.retweets > elem.retweets)
    if (morePopular.isEmpty) elem else morePopular.mostRetweeted
  }
    
  /**
   * Следующие методы уже реализованы
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = TweetReader.allTweets.mentions(google)
  lazy val appleTweets: TweetSet = TweetReader.allTweets.mentions(apple)
  
  /**
   * Список всех твитов, упоминающих ключевое слово либо из списка apple или google,
   * отсортированные по количеству ретвитов.
   */
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
  }

object Main extends App {
  // Выводит на печать тренд твитов
  GoogleVsApple.trending foreach println
}