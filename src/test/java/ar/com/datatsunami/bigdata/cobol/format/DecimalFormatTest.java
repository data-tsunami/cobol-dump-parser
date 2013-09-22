package ar.com.datatsunami.bigdata.cobol.format;

import static org.junit.Assert.*;

import org.junit.Test;

public class DecimalFormatTest {

	@Test
	public void testFormatString() throws InvalidFormatException {
		DecimalToFloatConverter df = new DecimalToFloatConverter(2);

		for (String value : new String[] { "123+", " 123+ ", " 123+" }) {
			assertEquals("Didn't format value: '" + value + "'", Float.valueOf((float) 1.23),
					df.format(value));
		}

		for (String value : new String[] { "123-", " 123- ", " 123-" }) {
			assertEquals("Didn't format value: '" + value + "'", Float.valueOf((float) -1.23),
					df.format(value));
		}

		for (String value : new String[] { "", " ", ".", "x1", "x", "1.0", "+123", "-123", "+1.23", "-1.23",
				"1.23+", "1.23-" }) {
			try {
				df.format(value);
				fail("Format didn't throw exception! Value: '" + value + "'");
			} catch (InvalidFormatException ife) {
			}
		}
	}

	@Test
	public void testCheckEmpty() throws InvalidFormatException {
		DecimalToFloatConverter df = new DecimalToFloatConverter(2);
		df.setValueForEmpty(Float.valueOf((float) 3.69));

		assertEquals(df.format(" "), Float.valueOf((float) 3.69));
		assertEquals(df.format(""), Float.valueOf((float) 3.69));
	}
}
