package ar.com.datatsunami.bigdata.cobol.linehandler;

import java.util.List;

import ar.com.datatsunami.bigdata.cobol.Field;

public interface LineHandler {

	public abstract void prepareLine(String line);

	public abstract String getValueForField(int field);

	public void setFields(List<Field<?>> fields);

}