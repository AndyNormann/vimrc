package no.uio.ifi.pascal2100.parser;
import no.uio.ifi.pascal2100.scanner.*;
import no.uio.ifi.pascal2100.parser.*;
import static no.uio.ifi.pascal2100.scanner.TokenKind.*;

public class FILE extends PascalSyntax{

    public static FILE parse(Scanner s){
        enterParser("FILE");

        s.skip();
        
        FILE retVal = new FILE(s.curLineNum());

        leaveParser("FILE");

        return retVal;
    }

    FILE(int lineNum){
        super(lineNum);
    }

    public String identify(){
        return "<FILE> at line " + lineNum;
    }
}

