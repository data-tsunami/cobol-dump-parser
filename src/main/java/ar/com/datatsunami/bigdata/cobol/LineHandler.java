package ar.com.datatsunami.bigdata.cobol;

public interface LineHandler {

	public abstract void prepareLine(String line);

	public abstract String getValueForField(int field);

}