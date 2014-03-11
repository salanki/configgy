CONFIGGY
========

Configgy is a library for handling config files for a scala
daemon. The idea is that it should be simple and straightforward, allowing
you to plug it in and get started quickly.

This repository offers a further developed version of Robeys [original deprecated version](https://github.com/robey/configgy).

## Modifications made to the original Configgy
### Added multiple inheritance:

	sub (inherit="a,b") {
			…  
	}
### Added simple groupings of values without actually creating a new configMap  
	upper (  
		value1 = 1  
		value2 = 2  
	)  
	
translates into keys:

	upper-value1  
	upper-value2

### Support for complex types in value assignments
	myVal = {
		a = 1
		b = 2
		c = ["A","B","C"]
	}

usage:

	config.getConfigValue("myVal")
	>> Some(ConfigObject(Map("a" -> ConfigInt(1), "b" -> ConfigInt(2), c -> ConfigList(ConfigString("A"), ConfigString("B"), ConfigString("C"))))

### And a bunch of other stuff	
* Made including config files work as if the included file was actually just cut-and-pasted in the file it was included from
* Made searches for inherits recursive for multiple nesting level. Starts search from current level and then goes down each level to the root
* Made searches for variable substitutions recursive the same way as inherits.
* Scala 2.10 support
* Removed JMX and Logging
