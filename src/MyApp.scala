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
  println(mapdata)

  var basket: Map[String, Float] = Map()

  // define menu options as a Map of actions
  // for each menu item:
  // key is an Int, the value that will be read from the input
  // value is a function () => Boolean, i.e. no params and returns Boolean
  val actionMap = Map[Int, () => Boolean](
    1 -> handleCurrentPrices,
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
  def handleCurrentPrices(): Boolean = {
    mnuShowMapResult(getCurrentPrices)
    true
  }

  def handleHighestLowest(): Boolean = {
    mnuShowHighestLowest(getHighestLowestPrices)
    true
  }

  def handleMedian(): Boolean = {
    mnuShowMapResult(getMedianPrices)
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
  def median(list: List[Int]): Double = {
    val sortedList = list.sorted
    val size = sortedList.size
    if (size % 2 == 0) {
      //even number of elements, average the middle two
      (sortedList(size / 2 - 1) + sortedList(size / 2)) / 2.0
    } else {
      //odd number of elements, take the middle one
      sortedList(size / 2).toDouble
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

  // *******************************************************************************************************************
  // FUNCTIONS THAT INVOKE ACTION AND INTERACT WITH USER
  // each of these functions accepts user input if required for an operation,
  // invokes the relevant operation function and displays the results



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

  def getMedianPrice(food: String): Option[Double] = {
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

  def calculateBasketTotal(basket: Map[String, Float]): (Float, Map[String, Float]) = {
    val itemPrices = basket.flatMap { case (food, quantity) =>
      getCurrentPrice(food).map(price => food -> (price * quantity))
    }

    val total = itemPrices.values.sum

    (total, itemPrices)
  }

  // *******************************************************************************************************************

}
