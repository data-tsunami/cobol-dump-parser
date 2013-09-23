package ar.com.datatsunami.bigdata.cobol.linehandler;

import java.util.List;

import org.apache.hadoop.io.BytesWritable;
import org.apache.hadoop.io.Text;

import ar.com.datatsunami.bigdata.cobol.field.Field;

/**
 * Handles a line of text, extract values from each line.
 * 
 * @author Horacio G. de Oro
 * 
 */
public interface LineHandler {

	/**
	 * Set the list of field each line should have.
	 * 
	 * @param fields
	 */
	public void setFields(List<Field<?, ?>> fields);

	/**
	 * Prepares the handler to process a line.
	 * 
	 * @param line
	 */
	public abstract void prepareLine(String line);

	/**
	 * Prepares the handler to process a line.
	 * 
	 * @param line
	 */
	public abstract void prepareText(Text line);

	/**
	 * Get the value of a field from the line beeing processed.
	 * 
	 * @param field
	 * @return
	 */
	public abstract String getValueForField(int field);

	/**
	 * Copy the value of a field to the output instance.
	 * 
	 * @param field
	 * @param output
	 */
	public void copyValue(int field, Text output);

	/**
	 * Copy the bytes of a field to the BytesWritable instance.
	 * 
	 */
	public void copyBytes(int field, BytesWritable bytesWritable);

	//
	// If someday Pig support UDF from static methods, this will be a really
	// good idea. But since Pig doesn't support anything like that, it makes no
	// sense to put this functionallity here.
	//
	// /*
	// * Generates a Pig UDF instance to load the data
	// *
	// * @return
	// */
	// public LoadFunc getUDF();
	//
}