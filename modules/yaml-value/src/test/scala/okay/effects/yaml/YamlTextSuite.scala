package okay.effects.yaml

import okay.effects.json.Json

/** YAML as configurations are written: to the JSON value, a failure saying where, and back */
class YamlTextSuite extends munit.FunSuite {
  def read(t: String): Json = YamlText.parse(t).fold(e => fail(s"$e"), identity)
  def j(t: String): Json = Json.fromText(t)

  test("a compose file: mappings, sequences, flow, quoted, comments") {
    val v = read("""
      |# a compose file
      |services:
      |  web:
      |    image: "nginx:1.27"   # the front
      |    ports: ["80:80", '443:443']
      |    depends_on:
      |    - api
      |  api:
      |    image: example/api:1.4
      |    environment:
      |      DATABASE_URL: postgres://db:5432/app
      |      DEBUG: false
      |      WORKERS: 4
      |      RATIO: 0.5
      |      EMPTY:
      |    deploy: {replicas: 3, labels: {tier: back}}
      |""".stripMargin)
    assertEquals(v, j("""{"services": {"web": {"image": "nginx:1.27", "ports": ["80:80", "443:443"],
      "depends_on": ["api"]}, "api": {"image": "example/api:1.4", "environment": {"DATABASE_URL":
      "postgres://db:5432/app", "DEBUG": false, "WORKERS": 4, "RATIO": 0.5, "EMPTY": null},
      "deploy": {"replicas": 3, "labels": {"tier": "back"}}}}}"""))
  }

  test("Kubernetes manifests: documents, nested sequences of mappings") {
    val docs = YamlText.parseAll("""
      |apiVersion: apps/v1
      |kind: Deployment
      |metadata: {name: shop}
      |spec:
      |  replicas: 2
      |  template:
      |    spec:
      |      containers:
      |        - name: shop
      |          image: example/shop
      |          env:
      |            - name: DB_HOST
      |              value: shop-db
      |            - {name: MODE, value: "on"}
      |---
      |apiVersion: v1
      |kind: Service
      |metadata:
      |  name: shop
      |---
      |""".stripMargin).fold(e => fail(s"$e"), identity)
    assertEquals(docs.size, 2)
    assertEquals(docs(0)("spec")("template")("spec")("containers").list.head("env").list.map(_("value").string),
      Vector("shop-db", "on"))
    assertEquals(docs(1)("metadata")("name").string, "shop")
  }

  test("anchors, aliases and the merge key") {
    val v = read("""
      |x-common: &common
      |  restart: always
      |  image: base
      |a:
      |  <<: *common
      |  image: mine
      |b: *common
      |list: [&one 1, *one]
      |""".stripMargin)
    assertEquals(v("a"), j("""{"restart": "always", "image": "mine"}"""))
    assertEquals(v("b"), j("""{"restart": "always", "image": "base"}"""))
    assertEquals(v("list"), j("[1, 1]"))
  }

  test("block scalars: literal, folded, chomping") {
    val v = read("""
      |literal: |
      |  line one
      |    indented
      |  line three
      |
      |folded: >
      |  a long
      |  sentence
      |
      |  a paragraph
      |strip: |-
      |  no newline
      |keep: |+
      |  kept
      |
      |list:
      |  - |
      |    in a list
      |""".stripMargin)
    assertEquals(v("literal").string, "line one\n  indented\nline three\n")
    assertEquals(v("folded").string, "a long sentence\na paragraph\n")
    assertEquals(v("strip").string, "no newline")
    assertEquals(v("keep").string, "kept\n\n")
    assertEquals(v("list").list.map(_.string), Vector("in a list\n"))
  }

  test("scalars by the core schema, quoting, tags, plain text continued") {
    val v = read("""
      |n: [1, -2, +3, 0o17, 0x1F, 1.5, 2e3, .inf, ~, null, true, False, "true", '12', !!str 12, 3.10]
      |s: 'it''s' 
      |d: "tab\there \u00e9"
      |url: http://x:8080/a#frag
      |long: a plain
      |  text on two lines
      |""".stripMargin)
    assertEquals(v("n"), j("""[1, -2, 3, 15, 31, 1.5, 2e3, ".inf", null, null, true, false, "true", "12", "12",
      3.10]"""))
    assertEquals(v("s").string, "it's")
    assertEquals(v("d").string, "tab\there é")
    assertEquals(v("url").string, "http://x:8080/a#frag")
    assertEquals(v("long").string, "a plain text on two lines")
  }

  test("a top-level sequence, a sequence under a key at the key's indentation, nested dashes") {
    assertEquals(read("- a\n- - b\n  - c\n- d: 1\n  e: 2\n"), j("""["a", ["b", "c"], {"d": 1, "e": 2}]"""))
    assertEquals(read("k:\n- 1\n- 2\nm: x\n"), j("""{"k": [1, 2], "m": "x"}"""))
    assertEquals(read(""), Json.Null)
    assertEquals(read("just text"), Json.Str("just text"))
  }

  test("a failure: where, and what was expected") {
    assertEquals(YamlText.parse("a: [1, 2\nb: 3").left.map(_.expected), Left("',' or ']'"))
    assertEquals(YamlText.parse("a: 1\n  b: 2").left.map(_.offset), Left(7))
    assertEquals(YamlText.parse("a: *nope").left.map(_.expected), Left("an anchor &nope defined before"))
    assertEquals(YamlText.parse("a: \"open").left.map(_.expected), Left("the closing \""))
    assert(YamlText.parse("a: 1\n---\nb: 2").isLeft)
  }

  test("printed and read back: the same value (random values)") {
    val rnd = new scala.util.Random(7)
    val words = Vector("a", "true", "1", "x: y", "#no", "", " sp", "-", "null", "é", "line\nbreak", "3.10", "<<",
      "k#v", "it's", "\"q\"", "[a]", "~", ".5")
    def value(d: Int): Json = rnd.nextInt(if (d > 3) 4 else 6) match {
      case 0 => Json.Str(words(rnd.nextInt(words.size)))
      case 1 => Json.Num(BigDecimal(rnd.nextInt(2000) - 1000) / 10)
      case 2 => Json.Bool(rnd.nextBoolean())
      case 3 => Json.Null
      case 4 => Json.Arr(Vector.fill(rnd.nextInt(4))(value(d + 1)))
      case _ => Json.Obj(Vector.fill(rnd.nextInt(4))(words(rnd.nextInt(words.size)) -> value(d + 1))
          .groupBy(_._1).map(_._2.head).toVector)
    }
    (1 to 500).foreach { _ =>
      val v = value(0)
      val text = YamlText.print(v)
      assertEquals(YamlText.parse(text), Right(v), text)
    }
  }
}
