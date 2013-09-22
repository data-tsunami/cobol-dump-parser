package ar.com.datatsunami.bigdata.cobol.converter;

import static org.junit.Assert.*;

import org.junit.Test;

import ar.com.datatsunami.bigdata.cobol.converter.DecimalToLongConverter;
import ar.com.datatsunami.bigdata.cobol.converter.InvalidFormatException;

public class LongBasedDecimalFormatTest {

	@Test
	public void testFormatString() throws InvalidFormatException {
		DecimalToLongConverter lbdf = new DecimalToLongConverter(3);
		assertEquals(Long.valueOf(87401239), lbdf.convert("87401239+"));
		assertEquals(Long.valueOf(-87401239), lbdf.convert("87401239-"));

		DecimalToLongConverter lbdf2 = new DecimalToLongConverter(3, false);
		assertEquals(Long.valueOf(87401239), lbdf2.convert("87401239"));
	}

	@Test(expected = InvalidFormatException.class)
	public void testFormatStringInvalid() throws InvalidFormatException {
		DecimalToLongConverter lbdf2 = new DecimalToLongConverter(3, false);
		assertEquals(Long.valueOf(87401239), lbdf2.convert("87401239+"));
	}

	@Test(expected = InvalidFormatException.class)
	public void testFormatStringInvalid2() throws InvalidFormatException {
		DecimalToLongConverter lbdf2 = new DecimalToLongConverter(3, false);
		assertEquals(Long.valueOf(87401239), lbdf2.convert("87401239-"));
	}

}
