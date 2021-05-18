package lectures.part3fp

import scala.annotation.tailrec

object TuplesAndMaps extends App {
  //tuples = finite order "lists"
  val aTuple = Tuple2(2, "Hello, scala") //Tuple2[Int,String] = (Int,String)
  println(aTuple._1) //2
  println(aTuple.copy(_2 = "Good bye Java"))
  println(aTuple.swap) //("Hello, scala",2)

  //Maps = key -> value
  val aMap: Map[String, Int] = Map()
  val phonebook: Map[String, Int] = Map(("Joe", 2333), "Daniel" -> 1245).withDefaultValue(-1)
  // a-> b is sugar for (a,b)
  println(phonebook)

  //map ops
  println(phonebook.contains("Joe"))
  println(phonebook("Joen"))

  //add pairing
  val newPairing = "Marry" -> 678
  val newPhonebook = phonebook + newPairing
  println(newPhonebook)

  //functionals on maps
  //map, filter, flatmap
  println(phonebook.map(pair => pair._1.toLowerCase -> pair._2))

  //filterKeys
  println(phonebook.filterKeys(x => x.startsWith("J")))

  //mapValues
  println(phonebook.mapValues(number => number * 10))

  //conversions to other collection
  println(phonebook.toList)
  println(List(("Daniel", 555)).toMap)

  val names = List("Bob", "James", "Angela", "Mary", "Daniel", "Jim")
  println(names.groupBy(name => name.charAt(0)))

  /*
    1. What would happen  if i had two original entires "Jim" -> 555 and  "JIM"->900?

    !!! careful while mapping values...
    
    2. Overly simplified social network based on maps
       Person -> String
        -- add a person to the network
        -- remove
        --friend (mutual)
        -- unfriend

        -- number of friends of a person
        -- person with most friends
        -- how many people have NO friends
        -- if there is social connection between two people (direct or not)

   */
  def add(network: Map[String, Set[String]], person: String): Map[String, Set[String]] = {
    network + (person -> Set())
  }

  def friend(network: Map[String, Set[String]], a: String, b: String): Map[String, Set[String]] = {
    val friendsA = network(a)
    val friendsB = network(b)

    network + (a -> (friendsA + b)) + (b -> (friendsB + a))
  }

  def unfriend(network: Map[String, Set[String]], a: String, b: String): Map[String, Set[String]] = {
    val friendsA = network(a)
    val friendsB = network(b)

    network + (a -> (friendsA - b)) + (b -> (friendsB - a))
  }

  def remove(network: Map[String, Set[String]], person: String): Map[String, Set[String]] = {
    @tailrec
    def removeAux(friends: Set[String], networkAcc: Map[String, Set[String]]): Map[String, Set[String]] = {
      if (friends.isEmpty) networkAcc
      else removeAux(friends.tail, unfriend(networkAcc, person, friends.head))
    }

    val unfriended = removeAux(network(person), network)
    unfriended - person
  }

  val empty: Map[String, Set[String]] = Map()
  val network = add(add(empty, "Bob"), "Mary")
  println(network)
  println(friend(network, "Bob", "Mary"))
  println(unfriend(friend(network, "Bob", "Mary"), "Bob", "Mary"))
  println(remove(friend(network, "Bob", "Mary"), "Bob"))

  //Jim , Bob,Mary
  val people = add(add(add(empty, "Bob"), "Mary"), "Jim")
  val jimBob = friend(people, "Bob", "Mary")
  val testNet = friend(jimBob, "Bob", "Jim")
  println(testNet)

  //number of friends of a person
  def nFriends(network: Map[String, Set[String]], person: String): Int =
    if (!network.contains(person)) 0
    else network(person).size

  println(nFriends(testNet, "Bob"))

  //Person with most friends
  def mostFriends(network: Map[String, Set[String]]): String =
    network.maxBy(pair => pair._2.size)._1

  println(mostFriends(testNet))

  //how many people have NO friends
  def noFriends(network: Map[String, Set[String]]): Set[String] =
    network.filter(n => n._2.isEmpty).keySet

  println(noFriends(add(testNet, "Sapna")))

  def nPeopleWithNoFriends(network: Map[String, Set[String]]): Int =
    //network.filterKeys(k => network(k).isEmpty).size //1st approach
    //network.filter(n => n._2.size == 0).size // 2nd approach
    network.count(n => n._2.isEmpty)

  println(nPeopleWithNoFriends(testNet))

  //if there is social connection between two people (direct or not)
  def socialConnection(network:Map[String,Set[String]],a:String,b:String):Boolean ={
      //Breath First Search
    @tailrec
    def bfs(target:String,consideredPeople:Set[String],discoveredPeople:Set[String]):Boolean ={
        if (discoveredPeople.isEmpty) false
        else{
          val person = discoveredPeople.head
          if(person == target) true
          else if(consideredPeople.contains(person)) bfs(target,consideredPeople,discoveredPeople.tail)
          else bfs(target,consideredPeople+person,discoveredPeople.tail++network(person))
        }
      }
    bfs(b,Set(),network(a) + a)
  }

  println("testnet = "+testNet)
  println("network ="+network)
  println(socialConnection(testNet,"Jim","Mary"))
  println(socialConnection(network,"Bob","Mary"))
}
