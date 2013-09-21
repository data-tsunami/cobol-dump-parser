package ar.com.datatsunami.pig;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import org.apache.commons.lang3.StringUtils;

/**
 * 
 * @author Horacio G. de Oro
 * 
 */
public class StaticMethodCaller {

	/**
	 * Calls a static method.
	 * 
	 * The method is referenced with the format:
	 * 
	 * parts.of.the.package.ClassName.functionName
	 * 
	 * @param staticFunction
	 * @return
	 */
	public static Object call(String staticFunction) {

		final String tokens[] = staticFunction.split("\\.");
		if (tokens.length < 3)
			throw new RuntimeException("Couldn't split by '.' - staticFunction: '" + staticFunction + "'");

		final String className = StringUtils.join(tokens, ".", 0, tokens.length - 1);
		final String functionName = tokens[tokens.length - 1];

		Class<?> clazz;
		try {
			clazz = StaticMethodCaller.class.getClassLoader().loadClass(className);
		} catch (ClassNotFoundException e) {
			throw new RuntimeException("ClassNotFoundException detected for class '" + className + "'", e);
		}

		Method staticMethod;
		try {
			staticMethod = clazz.getMethod(functionName);
		} catch (SecurityException e) {
			throw new RuntimeException("SecurityException detected when trying to get method '"
					+ functionName + "'", e);
		} catch (NoSuchMethodException e) {
			throw new RuntimeException("NoSuchMethodException detected when trying to get method '"
					+ functionName + "'", e);
		}

		if (!Modifier.isStatic(staticMethod.getModifiers()))
			throw new RuntimeException("Method '" + staticMethod + "' found, but isn't static!");

		Object ret;
		try {
			ret = staticMethod.invoke(null);
		} catch (IllegalArgumentException e) {
			throw new RuntimeException("IllegalArgumentException detected when invoking method '"
					+ staticMethod + "'", e);
		} catch (IllegalAccessException e) {
			throw new RuntimeException("IllegalAccessException detected when invoking method '"
					+ staticMethod + "'", e);
		} catch (InvocationTargetException e) {
			throw new RuntimeException("InvocationTargetException detected when invoking method '"
					+ staticMethod + "'", e);
		}

		return ret;
	}
}
