cobol-dump-parser
=================

Parser for Cobol dumps

## How to build

To build using Maven:

    $ mvn clean package

## Example

If you have the following Cobol structure:

    ITEMID PIC 9(6).
    CODE PIC X(5).
    DESCRIPTION PIC X(15).
    PRICE PIC S9(5)V99.
    INDEX PIC 9(3)V999.

and you dump the data to a plain file, each line will have 40 characters. For example:

    002541PTRYYFilm 8mm x 7mm 0007199+001500

To build a parser for that structure:

    CobolDumpParser cp = new CobolDumpParser();
    cp.add(new Field<Long>(6, "ItemID", new LongFormat()));
    cp.add(new Field<String>(5, "Code"));
    cp.add(new Field<String>(15, "Description"));
    cp.add(new Field<Float>(8, "Price", new DecimalFormat(2, true)));
    cp.add(new Field<Float>(6, "Index", new DecimalFormat(3, false)));

To parse a line:

    Map<String, Object> fields = cp.getItemsWithLabels(line);

To get the data:

    fields.get("ItemID")      -> returns a Long
    fields.get("Description") -> returns a String
    fields.get("Price")       -> returns a Float

## TODO

- Translate variable names, exception menssages, etc.
