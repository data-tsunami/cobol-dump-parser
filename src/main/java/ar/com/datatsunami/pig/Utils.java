package ar.com.datatsunami.pig;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class Utils {

	static final Log logger = LogFactory.getLog(Utils.class);

	/*
	 * Transform 3 long to a float
	 */
	public static float toFloat(long intPart, long decimalPart, long decimalPlaces) {
		if (decimalPart == 0 || decimalPlaces == 0) {
			return (float) intPart;
		}
		long divisor = (long) Math.pow(10, decimalPlaces);
		return ((float) intPart) + (((float) decimalPart) / ((float) divisor));
	}

	/*
	 * Try to get a long representing the sign (+1 or -1).
	 * 
	 * Returns '+1' if the sign is '+'.
	 * 
	 * Returns '-1' if the sign is '-'.
	 * 
	 * Returns '0' if can't get a sign.
	 */
	public static float getSign(Object object) throws NumberFormatException {

		if (String.class.isAssignableFrom(object.getClass())) {
			//
			// Object is a String, check value and return
			//
			if ("+".equals(object))
				return 1f;
			if ("-".equals(object))
				return -1f;
			return 0f;
		}

		throw new NumberFormatException("Object isn't a String");

		/*
		 * Now that we have 'getArgToFuncMapping()', only valid objects should
		 * reach this function.
		 * 
		 * Leaving the old code here for reference, unti we are sure it's not
		 * needed.
		 */

		// if (logger.isDebugEnabled())
		// logger.debug("getSign(): object isn't the string '+' nor '-' - class: '"
		// + object.getClass()
		// + "' - object: '" + object + "'");
		//
		// logger.warn("Object for sign isn't a String instance");
		//
		// //
		// // Object isn't a String. If equals to '+' or '-', return
		// //
		// if ("+".equals(object))
		// return 1f;
		// if ("-".equals(object))
		// return -1f;
		//
		// //
		// // Try to get a String representing the object
		// //
		// String string;
		// try {
		// string = object.toString();
		// } catch (Exception e) {
		// if (logger.isDebugEnabled())
		// logger.debug("object.toString() raised an exception", e);
		// throw new
		// NumberFormatException("object.toString() raised the exception: " +
		// e);
		// }
		//
		// if ("+".equals(string))
		// return 1f;
		// if ("-".equals(string))
		// return -1f;
		//
		// return 0f;
	}

}
