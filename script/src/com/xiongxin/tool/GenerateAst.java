package com.xiongxin.tool;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;

public class GenerateAst {

    public static void main(String[] args) throws IOException {
        if (args.length != 1) {
            System.err.println("Usage: generate_ast <output directory>");
            System.exit(64);
        }

        var outputDir = args[0];

        defineAst(outputDir, "Expr", Arrays.asList(
                "Binary  : Expr left, Token operator, Expr right",
                "Group   : Expr expression",
                "Literal : Object value",
                "Unary   : Token operator, Expr right"
        ));
    }

    private static void defineAst(
            String output, String baseName, List<String> types) throws IOException {
        var path = output + "/" + baseName + ".java";
        var writer = new PrintWriter(path, StandardCharsets.UTF_8);

        writer.println("package com.xiongxin.lox;");
        writer.println();
        writer.println("import java.util.List;");
        writer.println();
        writer.println("abstract class " + baseName + " {");

        writer.println("}");
        writer.close();
    }
}
