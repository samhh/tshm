module TSHM.ParserSpec (spec) where

import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range
import           Prelude
import           TSHM.Parser
import           TSHM.TypeScript
import           Test.Hspec
import           Test.Hspec.Hedgehog   (PropertyT, forAll, hedgehog, (===))
import           Test.Hspec.Megaparsec
import           Text.Megaparsec       (ParseErrorBundle, Parsec, eof, parse)

unlines' :: [String] -> String
unlines' = intercalate "\n"

parse' :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
parse' = flip parse ""

(/=*) :: (Eq a, Show a) => (s -> Either e a) -> s -> PropertyT IO ()
f /=* a = first (const ()) (f a) === Left ()

(=*=) :: (Eq a, Eq e, Show a, Show e) => Either e a -> a -> PropertyT IO ()
a =*= b = a === Right b

typeArgs' :: [TExpr] -> NonEmpty TypeArg
typeArgs' = fmap (, Nothing) . fromList

spec :: Spec
spec = describe "TSHM.Parser" $ do
  describe "expr" $ do
    it "parses void" $ do
      parse' expr "void" `shouldParse` TVoid

    it "parses boolean" $ do
      parse' expr "true" `shouldParse` TBoolean True
      parse' expr "false" `shouldParse` TBoolean False

    it "parses primitive" $ do
      parse' expr "A" `shouldParse` TMisc "A"
      parse' expr "string" `shouldParse` TMisc "string"

    it "parses primitive with identifier starting with a known identifier" $ do
      parse' expr "void" `shouldParse` TVoid
      parse' expr "voidx" `shouldParse` TMisc "voidx"

    it "parses string literal" $ do
      parse' expr "'abc'" `shouldParse` TString "abc"

    it "parses template literal string" $ do
      parse' expr "``" `shouldParse` TTemplate []
      parse' expr "`abc`" `shouldParse` TTemplate [TemplateStr "abc"]
      parse' expr "`${any}`" `shouldParse` TTemplate [TemplateExpr TAny]
      parse' expr "`abc${any}xyz${void}${x}`" `shouldParse`
        TTemplate [TemplateStr "abc", TemplateExpr TAny, TemplateStr "xyz", TemplateExpr TVoid, TemplateExpr (TMisc "x")]
      parse' expr "`${Quantity | Color} fish`" `shouldParse`
        TTemplate [TemplateExpr (TBinOp BinOpUnion (TMisc "Quantity") (TMisc "Color")), TemplateStr " fish"]
      parse' expr "`${string & keyof T}Changed`" `shouldParse`
        TTemplate [TemplateExpr (TBinOp BinOpIntersection (TMisc "string") (TUnOp UnOpKeys (TMisc "T"))), TemplateStr "Changed"]
      parse' expr "`${string & keyof A}${`Changed`}`" `shouldParse`
        TTemplate [TemplateExpr (TBinOp BinOpIntersection (TMisc "string") (TUnOp UnOpKeys (TMisc "A"))), TemplateExpr (TTemplate [TemplateStr "Changed"])]
      parse' expr "`${string & keyof A}${`Ch${'ang'}ed`}`" `shouldParse`
        TTemplate [TemplateExpr (TBinOp BinOpIntersection (TMisc "string") (TUnOp UnOpKeys (TMisc "A"))), TemplateExpr (TTemplate [TemplateStr "Ch", TemplateExpr (TString "ang"), TemplateStr "ed"])]

    it "parses number literal" $ do
      parse' expr "-.123" `shouldParse` TNumber "-.123"

    it "parses special array syntax" $ do
      parse' expr "a[][]" `shouldParse`
        TGeneric "Array" (typeArgs' [TGeneric "Array" $ typeArgs' [TMisc "a"]])

    it "parses readonly" $ do
      parse' expr "readonly [readonly A, { readonly x: readonly B }]" `shouldParse`
        TUnOp UnOpReadonly (TTuple [TUnOp UnOpReadonly (TMisc "A"), TObject (ObjectLit [ObjectPair Immut Required (OKeyIdent "x", TUnOp UnOpReadonly (TMisc "B"))])])

    it "parses object reference" $ do
      parse' expr "A['key']" `shouldParse` TIndexedAccess (TMisc "A") (TString "key")
      parse' expr "A[\"key\"]" `shouldParse` TIndexedAccess (TMisc "A") (TString "key")
      parse' expr "true['k1']['k2']" `shouldParse`
        TIndexedAccess (TIndexedAccess (TBoolean True) (TString "k1")) (TString "k2")
      parse' expr "A[K]" `shouldParse` TIndexedAccess (TMisc "A") (TMisc "K")

    it "parses dot notation" $ do
      parse' expr "a.b.c.d" `shouldParse` TDotAccess (TDotAccess (TDotAccess (TMisc "a") "b") "c") "d"

    it "postfix operators play nicely together" $ do
      parse' expr "a['k1']['k2'][][].x['k3'][]" `shouldParse`
        TGeneric "Array" (typeArgs' [TIndexedAccess (TDotAccess (TGeneric "Array" (typeArgs' [TGeneric "Array" (typeArgs' [TIndexedAccess (TIndexedAccess (TMisc "a") (TString "k1")) (TString "k2")])])) "x") (TString "k3")])

    it "parses conditional types" $ do
      parse' expr "a extends b ? c : d extends e ? f : g" `shouldParse`
        TCond (TMisc "a") (TMisc "b") (TMisc "c") (TCond (TMisc "d") (TMisc "e") (TMisc "f") (TMisc "g"))

    it "parses function" $ do
      parse' expr "<A, B>(a: A) => B" `shouldParse`
         TLambda (Lambda (Just $ typeArgs' [TMisc "A", TMisc "B"]) [(Required, Normal, TMisc "A")] (TMisc "B"))

      parse' expr "(x: F<A, B>) => G<C, D>" `shouldParse`
         TLambda (Lambda Nothing [(Required, Normal, TGeneric "F" $ typeArgs' [TMisc "A", TMisc "B"])] (TGeneric "G" $ typeArgs' [TMisc "C", TMisc "D"]))

    it "parses infix operators" $ do
      parse' expr "A & B | C" `shouldParse` TBinOp BinOpIntersection (TMisc "A") (TBinOp BinOpUnion (TMisc "B") (TMisc "C"))

    it "parses keyof" $ do
      parse' expr "keyof A<B>" `shouldParse` TUnOp UnOpKeys (TGeneric "A" $ typeArgs' [TMisc "B"])
      parse' expr "keyofx" `shouldParse` TMisc "keyofx"

    it "parses typeof" $ do
      parse' expr "typeof x & y" `shouldParse` TBinOp BinOpIntersection (TUnOp UnOpReflection (TMisc "x")) (TMisc "y")
      parse' expr "typeofx" `shouldParse` TMisc "typeofx"

    it "parses parentheses and changes precedence accordingly" $ do
      parse' expr "(A & B) | C" `shouldParse` TBinOp BinOpUnion (TGrouped $ TBinOp BinOpIntersection (TMisc "A") (TMisc "B")) (TMisc "C")

  describe "lambda" $ do
    it "parses minimal viable function" $ do
      parse' lambda "() => void" `shouldParse` Lambda Nothing [] TVoid

    it "parses type arguments" $ do
      parse' lambda "<A, Either<void, B>>() => void" `shouldParse`
        Lambda (Just $ typeArgs' [TMisc "A", TGeneric "Either" $ typeArgs' [TVoid, TMisc "B"]]) [] TVoid

    it "parses params" $ do
      parse' lambda "(x: number, y: string) => void" `shouldParse`
        Lambda Nothing [(Required, Normal, TMisc "number"), (Required, Normal, TMisc "string")] TVoid

    it "parses return type" $ do
      parse' lambda "() => number" `shouldParse` Lambda Nothing [] (TMisc "number")

    it "parses curried function" $ do
      parse' lambda "() => () => void" `shouldParse` Lambda Nothing [] (TLambda (Lambda Nothing [] TVoid))

    it "supports extends clause in function generics" $ do
      parse' lambda "<B, A extends Array<B>>(f: <C extends number, D>(x: 'ciao') => string) => void" `shouldParse`
        Lambda
          ( Just $ typeArgs'
            [ TMisc "B"
            , TSubtype "A" (TGeneric "Array" $ typeArgs' [TMisc "B"])
            ]
          )
          [ (Required, Normal, TLambda (Lambda
            (Just $ typeArgs' [TSubtype "C" (TMisc "number"), TMisc "D"])
            [(Required, Normal, TString "ciao")]
            (TMisc "string")))
          ]
          TVoid

    it "parses complex function" $ do
      parse' lambda "<A, Array<Option<B>>>(x: number, y: <C>(z: C) => () => C) => () => void" `shouldParse`
        Lambda
          ( Just $ typeArgs'
            [ TMisc "A"
            , TGeneric "Array" $ typeArgs'
              [ TGeneric "Option" $ typeArgs'
                [ TMisc "B"
                ]
              ]
            ]
          )
          [ (Required, Normal, TMisc "number")
          , (Required, Normal, TLambda (Lambda
              (Just $ typeArgs' [TMisc "C"])
              [(Required, Normal, TMisc "C")]
              (TLambda (Lambda Nothing [] (TMisc "C")))))
          ]
          ( TLambda (Lambda Nothing [] TVoid))

  describe "object" $ do
    it "parses empty object" $ do
      parse' object "{}" `shouldParse` ObjectLit []

    it "parses non-empty flat object" $ do
      parse' object "{ a: 1, b: 'two' }" `shouldParse` ObjectLit [ObjectPair Mut Required (OKeyIdent "a", TNumber "1"), ObjectPair Mut Required (OKeyIdent "b", TString "two")]

    it "parses non-empty nested object" $ do
      parse' object "{ a: 1, b: { c: true }[] }" `shouldParse`
        ObjectLit [ObjectPair Mut Required (OKeyIdent "a", TNumber "1"), ObjectPair Mut Required (OKeyIdent "b", TGeneric "Array" $ typeArgs' [TObject (ObjectLit [ObjectPair Mut Required (OKeyIdent "c", TBoolean True)])])]

    it "parses mixed comma and semi-colon delimiters" $ do
      parse' object "{ a: number, b: string; c: boolean }" `shouldParse`
        ObjectLit [ObjectPair Mut Required (OKeyIdent "a", TMisc "number"), ObjectPair Mut Required (OKeyIdent "b", TMisc "string"), ObjectPair Mut Required (OKeyIdent "c", TMisc "boolean")]

    it "parses different key types" $ do
      parse' object "{ a: a, 'b': b, 3.3: c, ['d']: d, [e]: e, [f: number]: f }" `shouldParse`
        ObjectLit
        [ ObjectPair Mut Required (OKeyIdent "a", TMisc "a")
        , ObjectPair Mut Required (OKeyStr "b", TMisc "b")
        , ObjectPair Mut Required (OKeyNum "3.3", TMisc "c")
        , ObjectPair Mut Required (OKeyComputed (TString "d"), TMisc "d")
        , ObjectPair Mut Required (OKeyComputed (TMisc "e"), TMisc "e")
        , ObjectPair Mut Required (OKeyIndex (TMisc "number"), TMisc "f")
        ]

    it "parses optional and required properties" $ do
      parse' object "{ a: A, b?: B, c: C }" `shouldParse`
        ObjectLit [ObjectPair Mut Required (OKeyIdent "a", TMisc "A"), ObjectPair Mut Optional (OKeyIdent "b", TMisc "B"), ObjectPair Mut Required (OKeyIdent "c", TMisc "C")]

    it "parses alternative method syntax" $ do
      parse' object "{ f?<A>(): void }" `shouldParse`
        ObjectLit [ObjectPair Mut Optional (OKeyIdent "f", TLambda $ Lambda (Just $ typeArgs' [TMisc "A"]) [] TVoid)]

    it "parses trailing delimiter in non-empty object" $ do
      parse' object "{ a: number; b: string, }" `shouldParse`
        ObjectLit [ObjectPair Mut Required (OKeyIdent "a", TMisc "number"), ObjectPair Mut Required (OKeyIdent "b", TMisc "string")]

    it "parses mapped type" $ do
      parse' object "{ [K in A]: B }" `shouldParse`
        ObjectMapped Nothing Nothing ("K", TMisc "A", Nothing) (TMisc "B")

      parse' object "{ [K in A]: B; }" `shouldParse`
        ObjectMapped Nothing Nothing ("K", TMisc "A", Nothing) (TMisc "B")

    it "parses readonly modifiers" $ do
      parse' object "{ readonly [K in A]: B }" `shouldParse`
        ObjectMapped (Just AddMut) Nothing ("K", TMisc "A", Nothing) (TMisc "B")

      parse' object "{ +readonly [K in A]: B }" `shouldParse`
        ObjectMapped (Just AddMut) Nothing ("K", TMisc "A", Nothing) (TMisc "B")

      parse' object "{ -readonly [K in A]: B }" `shouldParse`
        ObjectMapped (Just RemMut) Nothing ("K", TMisc "A", Nothing) (TMisc "B")

    it "parses optionality modifiers" $ do
      parse' object "{ [K in A]?: B }" `shouldParse`
        ObjectMapped Nothing (Just AddOpt) ("K", TMisc "A", Nothing) (TMisc "B")

      parse' object "{ [K in A]+?: B }" `shouldParse`
        ObjectMapped Nothing (Just AddOpt) ("K", TMisc "A", Nothing) (TMisc "B")

      parse' object "{ [K in A]-?: B }" `shouldParse`
        ObjectMapped Nothing (Just RemOpt) ("K", TMisc "A", Nothing) (TMisc "B")

    it "parses mapped type as clause" $ do
      parse' object "{ [K in A as B]: C }" `shouldParse`
        ObjectMapped Nothing Nothing ("K", TMisc "A", Just (TMisc "B")) (TMisc "C")

  describe "tuple" $ do
    it "parses empty tuple" $ do
      parse' tuple "[]" `shouldParse` []

    it "parses non-empty flat tuple" $ do
      parse' tuple "[a, 'b']" `shouldParse` [TMisc "a", TString "b"]

    it "parses non-empty nested tuple" $ do
      parse' tuple "[a, ['b', 3]]" `shouldParse` [TMisc "a", TTuple [TString "b", TNumber "3"]]

    it "parses non-empty tuple with trailing comma" $ do
      parse' tuple "[a,]" `shouldParse` [TMisc "a"]
      parse' tuple "[a, ]" `shouldParse` [TMisc "a"]
      parse' tuple "[a, b,]" `shouldParse` [TMisc "a", TMisc "b"]
      parse' tuple "[a, b, ]" `shouldParse` [TMisc "a", TMisc "b"]

  describe "num" $ do
    let p = num <* eof

    it "parses int" $ do
      parse' p "123" `shouldParse` "123"

    it "parses negative int" $ do
      parse' p "-123" `shouldParse` "-123"

    it "parses float" $ do
      parse' p ".123" `shouldParse` ".123"
      parse' p "123.456" `shouldParse` "123.456"

    it "parses negative float" $ do
      parse' p "-.123" `shouldParse` "-.123"
      parse' p "-123.456" `shouldParse` "-123.456"

    it "does not permit hyphen anywhere except start" $ do
      parse' p `shouldFailOn` "1-"
      parse' p `shouldFailOn` "1.2-"

    it "does not permit more than one period" $ do
      parse' p `shouldFailOn` ".1.2"
      parse' p `shouldFailOn` "1.2.3"

  describe "importDec" $ do
    let ident' = Gen.list (Range.linear 1 99) Gen.alpha

    it "parses default import" $ hedgehog $ do
      imp <- forAll ident'
      pkg <- forAll ident'
      parse' importDec ("import " <> imp <> " from \"" <> pkg <> "\"") =*=
        ImportDec pkg (ImportDef imp)

    it "parses default type import" $ hedgehog $ do
      imp <- forAll ident'
      pkg <- forAll ident'
      parse' importDec ("import type " <> imp <> " from \"" <> pkg <> "\"") =*=
        ImportDec pkg (ImportDef imp)

    it "parses named imports" $ hedgehog $ do
      imp1 <- forAll ident'
      imp2 <- forAll ident'
      pkg <- forAll ident'
      parse' importDec ("import { " <> imp1 <> ", " <> imp2 <> " } from \"" <> pkg <> "\"") =*=
        ImportDec pkg (ImportNamed $ fromList [imp1, imp2])

    it "parses named type imports" $ hedgehog $ do
      imp1 <- forAll ident'
      imp2 <- forAll ident'
      pkg <- forAll ident'
      parse' importDec ("import type { " <> imp1 <> ", " <> imp2 <> " } from \"" <> pkg <> "\"") =*=
        ImportDec pkg (ImportNamed $ fromList [imp1, imp2])

    it "parses asterisk imports" $ hedgehog $ do
      imp <- forAll ident'
      pkg <- forAll ident'
      parse' importDec ("import * as " <> imp <> " from \"" <> pkg <> "\"") =*=
        ImportDec pkg (ImportAll imp)

    it "parses mixed imports" $ hedgehog $ do
      imp1 <- forAll ident'
      imp2 <- forAll ident'
      imp3 <- forAll ident'
      pkg <- forAll ident'
      parse' importDec ("import " <> imp1 <> ", { " <> imp2 <> ", " <> imp3 <> " } from \"" <> pkg <> "\"") =*=
        ImportDec pkg (ImportDefAndNamed imp1 $ fromList [imp2, imp3])
      parse' importDec ("import " <> imp1 <> ", * as " <> imp2 <> " from \"" <> pkg <> "\"") =*=
        ImportDec pkg (ImportDefAndAll imp1 imp2)

    it "parses mixed type imports" $ hedgehog $ do
      imp1 <- forAll ident'
      imp2 <- forAll ident'
      imp3 <- forAll ident'
      pkg <- forAll ident'
      parse' importDec ("import type " <> imp1 <> ", { " <> imp2 <> ", " <> imp3 <> " } from \"" <> pkg <> "\"") =*=
        ImportDec pkg (ImportDefAndNamed imp1 $ fromList [imp2, imp3])

  describe "exportDec" $ do
    it "parses any default export" $ do
      parse' exportDec "export default x" =*= ExportDef (TMisc "x")
      parse' exportDec "export default x;" =*= ExportDef (TMisc "x")
      parse' exportDec "export default a & \"b\"" =*= ExportDef (TBinOp BinOpIntersection (TMisc "a") (TString "b"))
      parse' exportDec "export default a & \"b\";" =*= ExportDef (TBinOp BinOpIntersection (TMisc "a") (TString "b"))

  describe "constDecIdent" $ do
    let ident' = Gen.list (Range.linear 1 99) Gen.alpha

    it "parses const declaration" $ hedgehog $ do
      x <- forAll ident'
      parse' constDecIdent ("declare const " <> x <> ": ") =*= x

    it "parses exported const declaration" $ hedgehog $ do
      x <- forAll ident'
      parse' constDecIdent ("export declare const " <> x <> ": ") =*= x

    it "requires declaration" $ hedgehog $ do
      x <- forAll ident'
      parse' constDecIdent /=* ("const " <> x <> ": ")

  describe "typeArgs'" $ do
    it "parses single flat type argument" $ do
      parse' typeArgs "<A>" `shouldParse`
        typeArgs' [ TMisc "A"
        ]

    it "parses multiple flat type arguments" $ do
      parse' typeArgs "<A, B, C>" `shouldParse`
        typeArgs' [ TMisc "A"
        , TMisc "B"
        , TMisc "C"
        ]

    it "parses single nested type argument" $ do
      parse' typeArgs "<Array<Option<A>>>" `shouldParse`
        typeArgs' [ TGeneric "Array" $ typeArgs'
          [ TGeneric "Option" $ typeArgs'
            [ TMisc "A"
            ]
          ]
        ]

    it "parses mixed type arguments" $ do
      parse' typeArgs "<A, Array<Option<B>, C>, Either<E, A>>" `shouldParse`
        typeArgs'
          [ TMisc "A"
          , TGeneric "Array" $ typeArgs'
            [ TGeneric "Option" $ typeArgs'
              [ TMisc "B"
              ]
            , TMisc "C"
            ]
          , TGeneric "Either" $ typeArgs'
            [ TMisc "E"
            , TMisc "A"
            ]
          ]

    it "parses default type arguments" $ do
      parse' typeArgs "<A, B = C, D extends E = F<G>>" `shouldParse`
        fromList
          [ (TMisc "A", Nothing)
          , (TMisc "B", Just $ TMisc "C")
          , (TSubtype "D" (TMisc "E"), Just $ TGeneric "F" $ typeArgs' [TMisc "G"])
          ]

    it "requires at least one type argument" $ do
      parse' typeArgs `shouldFailOn` "<>"

    it "parses trailing comma in non-empty arguments list" $ do
      parse' typeArgs "<A,>" `shouldParse`
        typeArgs' [ TMisc "A"
        ]

  describe "params" $ do
    it "parses empty params" $ do
      parse' params "()" `shouldParse` []

    it "parses single param" $ do
      parse' params "(a: number)" `shouldParse` [(Required, Normal, TMisc "number")]

    it "parses multiple params" $ do
      parse' params "(a: number, b: <A>(x: string) => void, c: Array<boolean>)" `shouldParse`
        [ (Required, Normal, TMisc "number")
        , (Required, Normal, TLambda (Lambda (Just $ typeArgs' [TMisc "A"]) [(Required, Normal, TMisc "string")] TVoid))
        , (Required, Normal, TGeneric "Array" $ typeArgs' [TMisc "boolean"])
        ]

    it "parses rest params" $ do
      parse' params "(...a: number, b: string, ...c: boolean)" `shouldParse`
        [ (Required, Rest, TMisc "number")
        , (Required, Normal, TMisc "string")
        , (Required, Rest, TMisc "boolean")
        ]

    it "parses optional params" $ do
      parse' params "(a?: A, ...b?: B, c: C)" `shouldParse`
        [ (Optional, Normal, TMisc "A")
        , (Optional, Rest, TMisc "B")
        , (Required, Normal, TMisc "C")
        ]

    it "parses optional whitespace and newlines" $ do
      parse' params (unlines' ["(", ")"]) `shouldParse` []
      parse' params (unlines' ["(", "x: A", ")"]) `shouldParse` [(Required, Normal, TMisc "A")]
      parse' params (unlines' ["(", "x: A,", "y: B", ")"]) `shouldParse` [(Required, Normal, TMisc "A"), (Required, Normal, TMisc "B")]

    it "parses optional trailing comma in non-empty params" $ do
      parse' params "(a: number,)" `shouldParse` [(Required, Normal, TMisc "number")]

    it "parses destructures" $ do
      parse' params "({ a: [a, { a, ...a }, ...a], a: a, ...a }: x)" `shouldParse` [(Required, Normal, TMisc "x")]
      parse' params "([a, { a, ...a }, ...a]: x)" `shouldParse` [(Required, Normal, TMisc "x")]

  describe "alias" $ do
    let p = parse' $ alias <* eof

    it "optionally supports semicolons" $ do
      p "type X = Y" `shouldParse` Alias "X" Nothing (TMisc "Y")
      p "type X = Y;" `shouldParse` Alias "X" Nothing (TMisc "Y")

    it "parses type arguments" $ do
      p "type X<A, B extends string> = A | B" `shouldParse`
        Alias
          "X"
          (Just $ typeArgs' [TMisc "A", TSubtype "B" (TMisc "string")])
          (TBinOp BinOpUnion (TMisc "A") (TMisc "B"))

  describe "interface" $ do
    let p = parse' $ interface <* eof

    it "parses without extends" $ do
      p "interface X { a: A }" `shouldParse`
        Interface
          "X"
          Nothing
          Nothing
          (ObjectLit [ObjectPair Mut Required (OKeyIdent "a", TMisc "A")])

      p "interface X<A, B extends Array<A>> { a: A }" `shouldParse`
        Interface
          "X"
          (Just $ typeArgs' [TMisc "A", TSubtype "B" (TGeneric "Array" $ typeArgs' [TMisc "A"])])
          Nothing
          (ObjectLit [ObjectPair Mut Required (OKeyIdent "a", TMisc "A")])

    it "parses with extends" $ do
      p "interface X extends B { a: A }" `shouldParse`
        Interface
          "X"
          Nothing
          (Just $ TMisc "B")
          (ObjectLit [ObjectPair Mut Required (OKeyIdent "a", TMisc "A")])

      p "interface X<A, B extends Array<A>> extends C { a: A }" `shouldParse`
        Interface
          "X"
          (Just $ typeArgs' [TMisc "A", TSubtype "B" (TGeneric "Array" $ typeArgs' [TMisc "A"])])
          (Just $ TMisc "C")
          (ObjectLit [ObjectPair Mut Required (OKeyIdent "a", TMisc "A")])

  describe "constDec" $ do
    let p = parse' $ constDec <* eof

    it "optionally supports semicolons" $ do
      p "declare const x: string" `shouldParse` ConstDec "x" (TMisc "string")
      p "declare const x: string;" `shouldParse` ConstDec "x" (TMisc "string")

  describe "fnDec" $ do
    let p = parse' $ fnDec <* eof

    it "parses" $ do
      p "declare function f<A>(x: A): <B extends A>(y: B) => C" `shouldParse` FunctionDec "f" (Lambda (Just $ typeArgs' [TMisc "A"]) [(Required, Normal, TMisc "A")] (TLambda $ Lambda (Just $ typeArgs' [TSubtype "B" (TMisc "A")]) [(Required, Normal, TMisc "B")] (TMisc "C")))

  describe "enum" $ do
    let p = parse' $ enum <* eof

    it "parses" $ do
      p "enum X {}" `shouldParse` SEnum "X" []
      p "enum X { A, B }" `shouldParse` SEnum "X" [EnumMember (EKeyIdent "A") Nothing, EnumMember (EKeyIdent "B") Nothing]
      p "enum X { A = 0, B, 'C' = '1' }" `shouldParse` SEnum "X"
        [ EnumMember (EKeyIdent "A") (Just $ TNumber "0")
        , EnumMember (EKeyIdent "B") Nothing
        , EnumMember (EKeyStr "C") (Just $ TString "1")
        ]

  describe "signature" $ do
    it "parses and skips comments" $ do
      parse' signature "declare /*x*/const/**/ x/* x xx xxx */: void/**///x" `shouldParse` SignatureConstDec (ConstDec "x" TVoid)

    it "parses all variants" $ do
      parse' signature "declare const f: void" `shouldParse` SignatureConstDec (ConstDec "f" TVoid)
      parse' signature "export declare const f: void" `shouldParse` SignatureConstDec (ConstDec "f" TVoid)
      parse' signature "type X = void" `shouldParse` SignatureAlias (Alias "X" Nothing TVoid)
      parse' signature "export type X = void" `shouldParse` SignatureAlias (Alias "X" Nothing TVoid)
      parse' signature "interface X {}" `shouldParse` SignatureInterface (Interface "X" Nothing Nothing (ObjectLit []))
      parse' signature "export interface X {}" `shouldParse` SignatureInterface (Interface "X" Nothing Nothing (ObjectLit []))
      parse' signature "declare function f(): void" `shouldParse` SignatureFunctionDec (fromList [FunctionDec "f" (Lambda Nothing [] TVoid)])
      parse' signature "export declare function f(): void" `shouldParse` SignatureFunctionDec (fromList [FunctionDec "f" (Lambda Nothing [] TVoid)])
      parse' signature (unlines' ["declare function f(): A", "declare function f(): B"]) `shouldParse`
        SignatureFunctionDec (fromList
          [ FunctionDec "f" (Lambda Nothing [] (TMisc "A"))
          , FunctionDec "f" (Lambda Nothing [] (TMisc "B"))
          ])
      parse' signature (unlines' ["export declare function f(): A", "export declare function f(): B"]) `shouldParse`
        SignatureFunctionDec (fromList
          [ FunctionDec "f" (Lambda Nothing [] (TMisc "A"))
          , FunctionDec "f" (Lambda Nothing [] (TMisc "B"))
          ])

    it "parses real signatures" $ do
      parse' signature "export declare const empty: ''" `shouldParse` SignatureConstDec (ConstDec "empty" (TString ""))

      parse' signature "export declare const aperture: (n: number) => <A>(xs: A[]) => A[][]" `shouldParse`
        SignatureConstDec (ConstDec "aperture" (TLambda (Lambda Nothing [(Required, Normal, TMisc "number")] (TLambda (Lambda (Just $ typeArgs' [TMisc "A"]) [(Required, Normal, TGeneric "Array" $ typeArgs' [TMisc "A"])] (TGeneric "Array" $ typeArgs' [TGeneric "Array" $ typeArgs' [TMisc "A"]]))))))

      parse' signature "export declare const anyPass: <A>(fs: Predicate<A>[]) => Predicate<A>" `shouldParse`
        SignatureConstDec (ConstDec "anyPass" (TLambda (Lambda (Just $ typeArgs' [TMisc "A"]) [(Required, Normal, TGeneric "Array" $ typeArgs' [TGeneric "Predicate" $ typeArgs' [TMisc "A"]])] (TGeneric "Predicate" $ typeArgs' [TMisc "A"]))))

      parse' signature "export declare const merge: <A>(x: A) => <B>(y: B) => A & B" `shouldParse`
        SignatureConstDec (ConstDec "merge" (TLambda (Lambda (Just $ typeArgs' [TMisc "A"]) [(Required, Normal, TMisc "A")] (TLambda (Lambda (Just $ typeArgs' [TMisc "B"]) [(Required, Normal, TMisc "B")] (TBinOp BinOpIntersection (TMisc "A") (TMisc "B")))))))

      parse' signature "export declare const omit: <K extends string>(ks: K[]) => <V, A extends Record<K, V>>(x: Partial<A>) => Pick<A, Exclude<keyof A, K>>" `shouldParse`
        SignatureConstDec (ConstDec "omit" (TLambda (Lambda (Just $ typeArgs' [TSubtype "K" (TMisc "string")]) [(Required, Normal, TGeneric "Array" $ typeArgs' [TMisc "K"])] (TLambda (Lambda (Just $ typeArgs' [TMisc "V", TSubtype "A" (TGeneric "Record" $ typeArgs' [TMisc "K", TMisc "V"])]) [(Required, Normal, TGeneric "Partial" $ typeArgs' [TMisc "A"])] (TGeneric "Pick" $ typeArgs' [TMisc "A", TGeneric "Exclude" $ typeArgs' [TUnOp UnOpKeys (TMisc "A"), TMisc "K"]]))))))

      parse' signature "export declare const unary: <A extends unknown[], B>(f: (...xs: A) => B) => (xs: A) => B" `shouldParse`
        SignatureConstDec (ConstDec "unary" (TLambda (Lambda (Just $ typeArgs' [TSubtype "A" (TGeneric "Array" $ typeArgs' [TUnknown]), TMisc "B"]) [(Required, Normal, TLambda (Lambda Nothing [(Required, Rest, TMisc "A")] (TMisc "B")))] (TLambda (Lambda Nothing [(Required, Normal, TMisc "A")] (TMisc "B"))))))

      parse' signature "export interface Some<A> { readonly _tag: 'Some', readonly value: A }" `shouldParse`
        SignatureInterface (Interface "Some" (Just $ typeArgs' [TMisc "A"]) Nothing (ObjectLit [ObjectPair Immut Required (OKeyIdent "_tag", TString "Some"), ObjectPair Immut Required (OKeyIdent "value", TMisc "A")]))
