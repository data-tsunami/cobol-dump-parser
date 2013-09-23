package ar.com.datatsunami.pig;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class UnsignedDecimalToFloatTest {

	@Test
	public void testToFloat() {
		assertEquals(100.01f, UnsignedDecimalToFloat.toFloat(100l, 1l, 2l), 0.0001);
	}

}
