import org.scalatest.{ ConfigMap => ScalaTestConfigMap, _ }

import net.lag.configgy._
import configvalue._

abstract class UnitSpec extends WordSpec with Matchers

class ConfigSpec extends UnitSpec {
  "A Config" when {
    "loading an empty string" should {
      val config = Config.fromString("")

      "have zero keys" in {
        assert(config.keys.size == 0)
      }
    }

    "loading a string with garbage string" should {
      "throw a ParseException exception" in {
        intercept[ParseException] {
          val config = Config.fromString("sdasdas")

        }
      }
    }

    "working with an string" should {
      val config = new Config

      "set an int" in {
        config.setString("intValue", "string")
      }

      "return the correct string value" in {
        assert(config.getString("intValue") == Some("string"))
      }

      "return the correct string value even if key is in different case" in {
        assert(config.getString("IntValUe") == Some("string"))
      }

      "return a ConfigString if the String is requested as a ConfigValue" in {
        assert(config.getConfigValue("intValuE") == Some(ConfigString("string")))
      }

      "return the None if the string is requested as an Integer" in {
        assert(config.getInt("intValuE") == None)
      }
    }

    "working with an integer" should {
      val config = new Config

      "set an int" in {
        config.setInt("intValue", 100)
      }

      "return the correct integer value" in {
        assert(config.getInt("intValue") == Some(100))
      }

      "return the correct integer value even if key is in different case" in {
        assert(config.getInt("IntValUe") == Some(100))
      }

      "return the integer as string if requested as string" in {
        assert(config.getString("IntValUe") == Some("100"))
      }

      "return a Long if the Int is requested as a Long" in {
        assert(config.getLong("intValue") == Some(100L))
      }

      "return a ConfigInt if the Int is requested as a ConfigValue" in {
        assert(config.getConfigValue("intValuE") == Some(ConfigInt(100)))
      }
    }

    "working with a list of strings" should {
      val config = new Config

      "set an list of string" in {
        config.setList("intValue", List("a", "b"))
      }

      "return the correct string list value" in {
        assert(config.getList("intValue").toList == List("a", "b"))
      }

      "return the correct string list even if key is in different case" in {
        assert(config.getList("intValUE").toList == List("a", "b"))
      }

      "return a ConfigList of ConfigStrings if the StringList is requested as a ConfigValue" in {
        assert(config.getConfigValue("intValuE") == Some(ConfigList(ConfigString("a"), ConfigString("b"))))
      }

      "return the None if the string is requested as an Integer" in {
        assert(config.getInt("intValuE") == None)
      }
    }

    "working with a list of ConfigObjects" when {
      def testObjectList(config: ConfigMap) = {
        "return the correct ConfigObject list value" in {
          assert(config.getConfigValue("objList") == Some(ConfigList(ConfigObject("intValue" -> ConfigInt(1), "stringValue" -> ConfigString("b")))))
        }

        "return the correct ConfigObject list  even if key is in different case" in {
          assert(config.getConfigValue("objLISt") == Some(ConfigList(ConfigObject("intValue" -> ConfigInt(1), "stringValue" -> ConfigString("b")))))
        }

        "return None if the list is requested as String" in {
          assert(config.getString("objLISt") == None)
        }

        "convert to a ConfigString" in {
          assert(config.toConfigString == "objList = [{intValue = 1 stringValue = \"b\"}]]\n")
        }
      }

      "set via the API" should {
        val config = new Config

        "set a list of ConfigObjects" in {
          config.setConfigValue("objList", ConfigList(ConfigObject("intValue" -> ConfigInt(1), "stringValue" -> ConfigString("b"))))
        }

        testObjectList(config)
      }

      "read from a string" should {
        lazy val config = Config.fromString("objList = [{intValue = 1 stringValue = \"b\"}]")

        "load a list of ConfigObjects" in {
          config
        }
        testObjectList(config)
      }
    }

    "working with a list of ConfigObjects inside a ConfigOBject" when {
      def test(config: ConfigMap) = {
        "have one key" in {
          assert(config.keys.size == 1)
        }

        "have the list set in the object" in {
          config.getConfigValue("d") match {
            case Some(ConfigObject(a)) => a.get("h") match {
              case Some(ConfigList(a)) =>
              case other => assert(other == "Some(ConfigList(_)))")
            }
            case other => assert(other == "Some(ConfigObject(_))")
          }
        }
      }

      "read from a string" should {
        lazy val config = Config.fromString("d = {h = [{a = 1}]}")

        "parse the string" in {
          config
        }

        test(config)
      }
    }

    "loading a List of an integer, a boolean and a string inside a ConfigOBject" should {
      val config = Config.fromString("a = { b = [1,true,\"str\"]}")

      "have one key" in {
        assert(config.keys.size == 1)
      }
      
      "have the correct values and types" in {
        assert(config.configValue("a") == Some(ConfigObject(Map("b" -> ConfigList(ConfigInt(1), ConfigBoolean(true), ConfigString("str"))))))
      }
    }

    "loading a string with an integer and one string" should {
      val config = Config.fromString("intValue = 1\nstringValue = \"string\"")

      "have two keys" in {
        assert(config.keys.size == 2)
      }

      "return the correct integer value" in {
        assert(config.getInt("intValue") == Some(1))
      }

      "return the correct integer value even if key is in different case" in {
        assert(config.getInt("IntValUe") == Some(1))
      }

      "return the integer as string if requested as string" in {
        assert(config.getString("IntValUe") == Some("1"))
      }

      "return the None if the string is requested as an Integer" in {
        assert(config.getInt("stringValue") == None)
      }

      "return a Long if the Int is requested as a Long" in {
        assert(config.getLong("intValue") == Some(1L))
      }

      "convert to the correct ConfigString" in {
        assert(config.toConfigString == "stringValue = \"string\"\nintValue = 1\n")
      }
    }

    "working with an alias group with one alias level" should {
      lazy val config = Config.fromString("cli ( intValue = 1\nstringValue = \"string\" )")

      "parse a string with aliases" in {
        config
      }

      "have two keys" in {
        assert(config.keys.size == 2)
      }

      "return the correct integer value" in {
        assert(config.getInt("cli-intValue") == Some(1))
      }

      "return the correct integer value even if key is in different case" in {
        assert(config.getInt("cli-IntValUe") == Some(1))
      }

      "return the integer as string if requested as string" in {
        assert(config.getString("cli-IntValUe") == Some("1"))
      }

      "return the None if the string is requested as an Integer" in {
        assert(config.getInt("cli-stringValue") == None)
      }

      "return a Long if the Int is requested as a Long" in {
        assert(config.getLong("cli-intValue") == Some(1L))
      }

      "convert to the correct String" in {
        assert(config.toString == """{: cli ( stringValue="string" intValue=1 ) }""")
      }

      "convert to the correct ConfigString" in {
        assert(config.toConfigString == "cli (\n stringValue = \"string\"\n intValue = 1\n)\n")
      }
    }

    "working with an alias group with two alias levels" should {
      lazy val config = Config.fromString("cli ( intValue = 1\n extra (stringValue = \"string\" intValue = 2))")

      "parse a string with aliases" in {
        config
      }

      "have three keys" in {
        assert(config.keys.size == 3)
      }

      "return the correct first integer value" in {
        assert(config.getInt("cli-intValue") == Some(1))
      }

      "return the correct second integer value" in {
        assert(config.getInt("cli-extra-intValue") == Some(2))
      }

      "return the None if the string is requested as an Integer" in {
        assert(config.getInt("cli-extra-stringValue") == None)
      }

      "convert to the correct String" in {
        assert(config.toString == """{: cli ( extra ( stringValue="string" intValue=2 ) intValue=1 ) }""")
      }

      "convert to the correct ConfigString" in {
        //  println(config.toConfigString)
        assert(config.toConfigString == "cli (\n stringValue = \"string\"\n intValue = 1\n)\n")
      }
    }

    "working with an alias group with two alias levels inside a map" should {
      lazy val config = Config.fromString("root { cli ( intValue = 1\n extra (stringValue = \"string\" intValue = 2)) }")

      "parse a string with aliases" in {
        config
      }

      "have one key" in {
        assert(config.keys.size == 1)
      }

      "return the correct first integer value" in {
        assert(config.getInt("root.cli-intValue") == Some(1))
      }

      "return the correct second integer value" in {
        assert(config.getInt("root.cli-extra-intValue") == Some(2))
      }

      "convert to the correct String" in {
        println(config.toString)
        assert(config.toString == """{: root={root: cli ( extra ( stringValue="string" intValue=2 ) intValue=1 ) } }""")
      }

      "convert to the correct ConfigString" in {
        assert(config.toConfigString == """root {
  cli (
   extra (
    stringValue = "string"
    intValue = 2
   )
   intValue = 1
  )
}

""")
      }
    }

    /* Old tests */
    "include from a resource" in {
      val c = new Config
      c.importer = new ResourceImporter(getClass.getClassLoader)
      c.load("include \"happy.conf\"\n")
      assert(c.toString == "{: commie=501 }")
    }

    "build from a map" in {
      val c = Config.fromMap(Map("apples" -> "23", "oranges" -> "17", "fruit.misc" -> "x,y,z"))
      assert(c("apples") == "23")
      assert(c("oranges") == "17")
      assert(c("fruit.misc") == "x,y,z")

      assert(c.toString == """{: fruit={fruit: misc="x,y,z" } oranges=17 apples=23 }""")
      assert(c.configMap("fruit").getName == "fruit")
    }
  }
}