package clojure.data.json;

public class JSONStringBufferTest {


    public static void main(String[] args) throws Exception {
        System.out.println(new JSONStringBuffer("    1").skipWhiteSpace());
        System.out.println(new JSONStringBuffer("\"a string\"").string());
        System.out.println(new JSONStringBuffer("   \"a key with ws\"   :").keyString());
        System.out.println(new JSONStringBuffer("\"lol\":\"bla\"").keyString());
    }

}

