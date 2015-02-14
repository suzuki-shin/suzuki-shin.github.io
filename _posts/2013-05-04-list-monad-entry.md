---
layout: post
title: それリストモナドでできるよ
---

> 与えられた文字列から小文字/大文字を組み合わせたありうる組み合わせを列挙する関数書いた。1行で。

http://mudatobunka.org/2013/04/372

というのを見かけたのでぼくもやってみた。

> 例えば”Hail2U”という文字列を元にして、
> ["hail2u","hail2U","haiL2u","haiL2U","haIl2u","haIl2U","haIL2u","haIL2U"
> ,"hAil2u","hAil2U","hAiL2u","hAiL2U","hAIl2u","hAIl2U","hAIL2u","hAIL2U"
> ,"Hail2u","Hail2U","HaiL2u","HaiL2U","HaIl2u","HaIl2U","HaIL2u","HaIL2U"
> ,"HAil2u","HAil2U","HAiL2u","HAiL2U","HAIl2u","HAIl2U","HAIL2u","HAIL2U"]
> を吐き出す。

元記事の人も書いてるけど、これはまさにリストモナドの出番

```haskell
nub . mapM (\x -> [toLower x, toUpper x])
```

で

```haskell
> nub . mapM (\x -> [toLower x, toUpper x]) $ "Hail2U"
["hail2u","hail2U","haiL2u","haiL2U","haIl2u","haIl2U","haIL2u","haIL2U","hAil2u","hAil2U","hAiL2u","hAiL2U","hAIl2u","hAIl2U","hAIL2u","hAIL2U","Hail2u","Hail2U","HaiL2u","HaiL2U","HaIl2u","HaIl2U","HaIL2u","HaIL2U","HAil2u","HAil2U","HAiL2u","HAiL2U","HAIl2u","HAIl2U","HAIL2u","HAIL2U"]
```
