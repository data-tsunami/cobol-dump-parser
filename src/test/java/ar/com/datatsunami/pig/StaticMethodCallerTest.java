package ar.com.datatsunami.pig;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class StaticMethodCallerTest {

	public static String someStaticFunction() {
		return "the-string";
	}

	public static Integer someOtherStaticFunction() {
		return Integer.valueOf(5);
	}

	@Test
	public void testString() {
		Object ret = StaticMethodCaller.call("ar.com.datatsunami.pig.StaticMethodCallerTest"
				+ ".someStaticFunction");
		assertTrue(ret != null);
		assertTrue(String.class.isAssignableFrom(ret.getClass()));
		assertEquals("the-string", (String) ret);
	}

	@Test
	public void testInteger() {

		Object ret = StaticMethodCaller.call("ar.com.datatsunami.pig.StaticMethodCallerTest"
				+ ".someOtherStaticFunction");
		assertTrue(ret != null);
		assertTrue(Integer.class.isAssignableFrom(ret.getClass()));
		assertEquals(Integer.valueOf(5), (Integer) ret);
	}
}
