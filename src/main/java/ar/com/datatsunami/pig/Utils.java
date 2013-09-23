package ar.com.datatsunami.pig;

import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class Utils {

	static final Log logger = LogFactory.getLog(Utils.class);

	public static final Float NEGATIVE = new Float(-1);
	public static final Float POSITIVE = new Float(1);

	/*
	 * Transform 3 long to a float
	 */
	public static Float toFloat(long intPart, long decimalPart, long decimalPlaces) {
		if (intPart < 0 || decimalPart < 0 || decimalPlaces < 0)
			return null;

		if (decimalPart == 0 || decimalPlaces == 0) {
			return Float.valueOf(intPart);
		}

		float divisor = (float) Math.pow(10, decimalPlaces);
		return Float.valueOf(((float) intPart) + (((float) decimalPart) / divisor));
	}

	/*
	 * Try to get a long representing the sign (+1 or -1).
	 * 
	 * Returns '+1' if the sign is '+'.
	 * 
	 * Returns '-1' if the sign is '-'.
	 * 
	 * Returns 'null' if can't get a sign.
	 */
	public static Float getSign(Object object) throws IOException {

		if (String.class.isAssignableFrom(object.getClass())) {
			//
			// Object is a String, check value and return
			//
			if ("+".equals(object))
				return POSITIVE;
			if ("-".equals(object))
				return NEGATIVE;
			return null;
		}

		throw new IOException("Object to get the sign from is not a String");
	}

}
