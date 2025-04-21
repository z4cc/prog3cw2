/**
  * Created by jim on 06/11/2016.
  */

import scala.collection.immutable.ListMap
import scala.io.Source
import scala.io.StdIn.{readInt, readLine}
import scala.util.{Try, Success, Failure}
import scala.annotation.tailrec

object MyApp extends App {

  // *******************************************************************************************************************
  // application logic

  private val mapdata = readFile("data.txt")

  // Print status based on data load result
  if (mapdata.nonEmpty) {
    println("Data loaded successfully.")
    displayAvailableItems()
    runMenu()
  } else {
    println("Failed to load data. Exiting application.")
  }

  private def addToBasket(currentBasket: Map[String, Int], food: String, quantity: Int): Map[String, Int] = {
    currentBasket + (food -> (currentBasket.getOrElse(food, 0) + quantity))
  }

  // *******************************************************************************************************************
  // FUNCTIONS FOR MENU

  @tailrec
  private def runMenu(): Unit = {
    val option = readOption
    val shouldContinue = handleAction(option)
    if (shouldContinue) runMenu()
  }

  private def readOption: Int = {
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

    Try(readInt()).getOrElse {
      println("Invalid input. Please enter a number.")
      readOption //try again
    }
  }

  private def displayAvailableItems(): Unit = {
    println("\nAvailable food items:")
    println("-------------------")
    mapdata.keys.toList.sorted.foreach(println)
    println("-------------------")
  }

  //using a map to dispatch actions
  private def handleAction(option: Int): Boolean = {
    val actionMap = Map[Int, () => Boolean](
      1 -> handleCurrentPrice,
      2 -> handleHighestLowest,
      3 -> handleMedian,
      4 -> handleCompareAverage,
      5 -> handleBasket,
      6 -> handleQuit
    )

    actionMap.get(option) match {
      case Some(f) => f()
      case None =>
        println("Sorry, that command is not recognised")
        true
    }
  }

  // handlers for menu options
  private def handleCurrentPrice(): Boolean = {
    mnuShowFoodValue(getCurrentPrice, "Current price")
    true
  }

  private def handleHighestLowest(): Boolean = {
    mnuShowFoodValue(getHighestLowestPrices, "Highest and Lowest")
    true
  }

  private def handleMedian(): Boolean = {
    mnuShowFoodValue(getMedianPrice, "Median price")
    true
  }

  private def handleCompareAverage(): Boolean = {
    mnuCompareAverages(compareAveragePrices)
    true
  }

  private def handleBasket(): Boolean = {
    mnuBasketTotal(calculateBasketTotal)
    true
  }

  private def handleQuit(): Boolean = {
    println("Exiting application. Goodbye!")
    false
  }

  // *******************************************************************************************************************
  // UTILITY FUNCTIONS

  // reads data file - comma separated file
  private def readFile(filename: String): Map[String, List[Int]] = {
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
  private def median(list: List[Int]): Int = {
    val sortedList = list.sorted
    val size = sortedList.size

    if (size % 2 == 0) {
      val middleIndices = List(size/2 - 1, size/2)
      middleIndices.map(sortedList(_)).sum / 2
    } else {
      sortedList(size / 2)
    }
  }

  //helper to check if food exists and return its prices if it does
  private def getFoodPrices(food: String): Option[List[Int]] = {
    mapdata.get(food.toUpperCase)
  }

  //helper to calculate average
  private def average(list: List[Int]): Double = {
    if (list.isEmpty) 0.0 else list.sum.toDouble / list.length
  }

  //helper to convert pence to pounds and return formatted string
  private def penceToPounds(pence: Int): String = f"£${pence/100.0}%.2f"
  private def penceToPounds(pence: Double): String = f"£${pence / 100.0}%.2f"


  // *******************************************************************************************************************
  // FUNCTIONS THAT INVOKE ACTION AND INTERACT WITH USER
  // each of these functions accepts user input if required for an operation,
  // invokes the relevant operation function and displays the results

  // Generic function to display a single value for a specific food item with conversion to pounds
  private def mnuShowFoodValue[A](getValueFunc: String => Option[A], valueType: String): Unit = {
    println("Enter food item:")
    val food = readLine.toUpperCase

    getValueFunc(food) match {
      case Some(value: Int) => println(s"$food: $valueType is ${penceToPounds(value)}")
      case Some(value: Double) => println(s"$food: $valueType is ${penceToPounds(value)}")
      case Some(value: (Int, Int)) =>
        val (high, low) = value
        println(s"$food: Highest price: ${penceToPounds(high)}, Lowest price: ${penceToPounds(low)}")
      case Some(value) => println(s"$food: $valueType is $value")
      case None => println(s"Food '$food' not found.")
    }
  }


  private def mnuCompareAverages(f: (String, String) => (String, Double, String, Double)): Unit = {
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

  private def mnuBasketTotal(f: Map[String, Int] => (Int, Map[String, Int])): Unit = {
    val basket = buildBasket()
    showBasketTotal(f, basket)
  }

  private def buildBasket(): Map[String, Int] = {
    @tailrec
    def addItems(currentBasket: Map[String, Int]): Map[String, Int] = {
      println("Enter food item (or 'DONE' to finish):")
      val food = readLine.toUpperCase

      if (food == "DONE") {
        currentBasket
      } else {
        if (mapdata.contains(food)) {
          println("Enter quantity (kg/litres):")
          Try(readInt()) match {
            case Success(quantity) =>
              addItems(currentBasket + (food -> (currentBasket.getOrElse(food, 0) + quantity)))
            case Failure(_) =>
              println("Invalid quantity. Please enter a number.")
              addItems(currentBasket)
          }
        } else {
          println(s"Warning: Food item '$food' not recognised.")
          addItems(currentBasket)
        }
      }
    }

    println("\nBuild your basket:")
    addItems(Map())
  }

  private def showBasketTotal(f: Map[String, Int] => (Int, Map[String, Int]), basket: Map[String, Int]): Unit = {
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

  private def getCurrentPrice(food: String): Option[Int] = {
    getFoodPrices(food).map(_.last)
  }

  private def getHighestLowestPrices(food: String): Option[(Int, Int)] = {
    getFoodPrices(food).map(prices => (prices.max, prices.min))
  }

  private def getMedianPrice(food: String): Option[Int] = {
    getFoodPrices(food).map(median)
  }

  private def compareAveragePrices(food1: String, food2: String): (String, Double, String, Double) = {
    val avg1 = getFoodPrices(food1.toUpperCase).map(prices => average(prices)).getOrElse(0.0)
    val avg2 = getFoodPrices(food2.toUpperCase).map(prices => average(prices)).getOrElse(0.0)

    (food1.toUpperCase, avg1, food2.toUpperCase, avg2)
  }

  private def calculateBasketTotal(basket: Map[String, Int]): (Int, Map[String, Int]) = {
    val itemPrices = basket.flatMap { case (food, quantity) =>
      getCurrentPrice(food).map(price => food -> (price * quantity))
    }

    val total = itemPrices.values.sum

    (total, itemPrices)
  }
}

  // *******************************************************************************************************************

