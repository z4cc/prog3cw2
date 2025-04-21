/**
  * Created by jim on 06/11/2016.
  */

import scala.collection.immutable.ListMap
import scala.io.Source
import scala.io.StdIn.{readInt, readLine}

object MyApp extends App {

  // *******************************************************************************************************************
  // application logic

  // read data from file
  val mapdata = readFile("data.txt")
  // print data to check it's been read in correctly
  println("Data loaded successfully.")

  displayAvailableItems()

  var basket: Map[String, Int] = Map()

  // define menu options as a Map of actions
  // for each menu item:
  // key is an Int, the value that will be read from the input
  // value is a function () => Boolean, i.e. no params and returns Boolean
  val actionMap = Map[Int, () => Boolean](
    1 -> handleCurrentPrice,
    2 -> handleHighestLowest,
    3 -> handleMedian,
    4 -> handleCompareAverage,
    5 -> handleBasket,
    6 -> handleQuit
  )

  // loop to read input and invoke menu option
  // uses function readOption to show menu and read input
  // uses function menu to invoke menu action
  // will terminate if menu returns false
  var opt = 0
  do {
    opt = readOption
  } while (menu(opt))


  // *******************************************************************************************************************
  // FUNCTIONS FOR MENU

  // shows menu and reads input
  def readOption: Int = {
    println(
      """|
         |FOOD BASKET PRICES ANALYSIS
         |Please select one of the following:
         |  1 - Show current prices for a food
         |  2 - Show highest and lowest prices for a food
         |  3 - Show median price for a food
         |  4 - Compare average prices of two foods
         |  5 - Calculate food basket total
         |  6 - Quit""".stripMargin)
    readInt()
  }

  def displayAvailableItems(): Unit = {
    println("\nAvailable food items:")
    println("-------------------")
    mapdata.keys.toList.sorted.foreach(food => println(food))
    println("-------------------")
  }

  // invokes selected menu option
  // finds corresponding function to invoke in action map using get
  // pattern matching used as get returns an Option
  def menu(option: Int): Boolean = {
    actionMap.get(option) match {
      case Some(f) => f()
      case None =>
        println("Sorry, that command is not recognized")
        true
    }
  }

  // handlers for menu options
  def handleCurrentPrice(): Boolean = {
    mnuShowFoodValue(getCurrentPrice, "Current price")
    true
  }

  def handleHighestLowest(): Boolean = {
    mnuShowFoodValue(getHighestLowestPrices, "Highest and Lowest")
    true
  }

  def handleMedian(): Boolean = {
    mnuShowFoodValue(getMedianPrice, "Median price")
    true
  }

  def handleCompareAverage(): Boolean = {
    mnuCompareAverages(compareAveragePrices)
    true
  }

  def handleBasket(): Boolean = {
    mnuBasketTotal(calculateBasketTotal)
    true
  }

  def handleQuit(): Boolean = {
    println("Exiting application. Goodbye!")
    false
  }

  // *******************************************************************************************************************
  // UTILITY FUNCTIONS

  // reads data file - comma separated file
  def readFile(filename: String): Map[String, List[Int]] = {
    // create buffer to build up map as we read each line
    var mapBuffer: Map[String, List[Int]] = Map()
    try {
      for (line <- Source.fromFile(filename).getLines()) {
        // for each line
        val splitline = line.split(",").map(_.trim).toList // split line at , and convert to List

        // add element to map buffer
        // splitline is line from file as List, e.g. List(Bayern Munich, 24)
        // use head as key
        // tail is a list, convert to int
        mapBuffer = mapBuffer ++ Map(splitline.head -> splitline.tail.map(_.toInt))

      }
    } catch {
      case ex: Exception => println("Sorry, an exception happened.")
    }
    mapBuffer
  }

  //helper to calculate median
  def median(list: List[Int]): Int = {
    val sortedList = list.sorted
    val size = sortedList.size
    if (size % 2 == 0) {
      //even number of elements, average the middle two
      (sortedList(size / 2 - 1) + sortedList(size / 2)) / 2.0.toInt
    } else {
      //odd number of elements, take the middle one
      sortedList(size / 2)
    }
  }

  //helper to check if food exists and return its prices if it does
  def exists(food: String): List[Int] = {
    mapdata.get(food.toUpperCase()) match {
      case Some(prices) => prices
      case None =>
        println(s"Food '$food' not found.")
        List(0)
    }
  }

  //helper to calculate average
  def average(list: List[Int]): Double = {
    list.sum.toDouble / list.length
  }

  //helper to convert pence to pounds and return formatted string
  def penceToPounds(pence: Int): String = f"£${pence/100.0}%.2f"
  def penceToPounds(pence: Double): String = f"£${pence / 100.0}%.2f"


  // *******************************************************************************************************************
  // FUNCTIONS THAT INVOKE ACTION AND INTERACT WITH USER
  // each of these functions accepts user input if required for an operation,
  // invokes the relevant operation function and displays the results

  // Generic function to display a single value for a specific food item with conversion to pounds
  def mnuShowFoodValue[A](getValueFunc: String => Option[A], valueType: String) = {
    println("Enter food item:")
    val food = readLine.toUpperCase
    getValueFunc(food) match {
      case Some(value: Int) => println(s"$food: $valueType is ${penceToPounds(value)}")
      case Some(value: Double) => println(s"$food: $valueType is ${penceToPounds(value)}")
      case Some(value: (Int, Int)) =>
        val (high, low) = value
        println(s"$food: Highest price: ${penceToPounds(high)}, Lowest price: ${penceToPounds(low)}")
      case Some(value) => println(s"$food: $valueType is $value")  // Fallback for other types
      case None => println(s"Food '$food' not found.")
    }
  }

  def mnuCompareAverages(f: (String, String) => (String, Double, String, Double)) = {
    println("Enter first food:")
    val food1 = readLine
    println("Enter second food:")
    val food2 = readLine

    val (name1, avg1, name2, avg2) = f(food1, food2)

    if (avg1 > 0 && avg2 > 0) {
      println(s"Average price for $name1: ${penceToPounds(avg1)}")
      println(s"Average price for $name2: ${penceToPounds(avg2)}")
      val difference = Math.abs(avg1 - avg2)
      val higherFood = if (avg1 > avg2) name1 else name2
      println(s"$higherFood is more expensive on average by ${penceToPounds(difference)}")
    }
  }

  def mnuBasketTotal(f: (Map[String, Int]) => (Int, Map[String, Int])) = {
    var continueAdding = true
    basket = Map() // Reset basket

    println("\nBuild your basket:")
    while (continueAdding) {
      println("Enter food item (or 'DONE' to finish):")
      val food = readLine.toUpperCase

      if (food == "DONE") {
        continueAdding = false
      } else {
        if (mapdata.contains(food)) {
          println("Enter quantity (kg/litres):")
          try {
            val quantity = readInt()
            basket = basket + (food -> (basket.getOrElse(food, 0) + quantity))
          } catch {
            case _: Exception => println("Invalid quantity. Please enter a number.")
          }
        } else {
          println(s"Warning: Food item '$food' not recognised.")
        }
      }
    }
    val (total, itemPrices) = f(basket)

    if (itemPrices.nonEmpty) {
      println("\nYour basket:")
      itemPrices.foreach { case (food, price) =>
        val currentPrice = getCurrentPrice(food).getOrElse(0)
        println(f"$food: ${basket(food)}%.2f kg/l at ${penceToPounds(currentPrice)} = ${penceToPounds(price)}")
      }
      println(s"\nTotal basket value: ${penceToPounds(total)}")
    } else {
      println("Basket is empty.")
    }
  }

  // *******************************************************************************************************************
  // OPERATION FUNCTIONS
  // each of these performs the required operation on the data and returns
  // the results to be displayed - does not interact with user

  def getCurrentPrice(food: String): Option[Int] = {
    val prices = exists(food)
    if (prices == List(0)) None else Some(prices.last)
  }

  def getHighestLowestPrices(food: String): Option[(Int, Int)] = {
    val prices = exists(food)
    if (prices == List(0)) None else Some((prices.max, prices.min))
  }

  def getMedianPrice(food: String): Option[Int] = {
    val prices = exists(food)
    if (prices == List(0)) None else Some(median(prices))
  }

  def compareAveragePrices(food1: String, food2: String): (String, Double, String, Double) = {
    val prices1 = exists(food1)
    val prices2 = exists(food2)

    val avg1 = if (prices1 == List(0)) 0.0 else average(prices1)
    val avg2 = if (prices2 == List(0)) 0.0 else average(prices2)

    (food1.toUpperCase, avg1, food2.toUpperCase, avg2)
  }

  def calculateBasketTotal(basket: Map[String, Int]): (Int, Map[String, Int]) = {
    val itemPrices = basket.flatMap { case (food, quantity) =>
      getCurrentPrice(food).map(price => food -> (price * quantity))
    }

    val total = itemPrices.values.sum

    (total, itemPrices)
  }

  // *******************************************************************************************************************

}
