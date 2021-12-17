# zd (zen docs)

Semantic knowledge base


* Knowledge base consists of structured resources.
* Each resource is file in zd format.
* Full name of resource calculated from project root dirictory: /myns/resource.zd => myns.resource
and may be referenced from other resources
* Resource may be described with zen tags



## Setup zen docs for your project

You have to have installed clojure cli. Go to the root of your project and run: 

```
echo '":title "My first zd/note"' > docs/readme.zd

clojure -Sdeps '{:deps {zen-lang/zd {:git/url "git@github.com:zen-lang/zd.git" :sha "2ffa5788e70d99c5ccd833a22266c70a6b4b90a1"}}}' -M -m zd.dev docs zrc
```

Open http://localhost:3030 and that's it. 

## Syntax

zd format is sugar for edn:

```edn
:title "Person"
:zen/tags #{foam.Person}
;; specila multiline string
:zd/desc md/
# Title
here is 

* any markdown

;; nested datastructure can be flatten with ~
:zd/book:intro
~:title "Title"
~:text md/

This is intro

:zd/book:examples
~:title "Example"
~:text md/

;; There is special syntax for vectors
:zd/comments:#
~:author aidbox.team.niquola
~:date "2021-11-11"
~:text md/

Comment 1

:zd/comments:#
~:author aidbox.team.niquola
~:date "2021-11-11"
~:text md/

Comment 2


Examples

;; direct hickup support


:aidbox/report
[:ul
 (->> (zd/by-tag 'incidents)
      (group-by :inci/customer))]
```

## TODO

^Eugeny&Max
* zd-mode for emacs
 * syntax
 * jump

^Vlad&Nicola
* markdown/zendown extensible down 
  * [[multi params params]]
  * code blocks with hl

* zd api (search)

* macro engine

* zen schema integration

* hickup engine
  * expressions
  * helpers - zd api search 
  * render helpers tables

^Eugeny&Max
* static generator
  * ui styles
  * render blocks  (annotations)
  * navigation (dsl & render)  

* dev-mode (watch file -> reload resource in db) -> render page

* plugin example
* import plugin

* sample knowledge base


```
git clone kb
cd kb
zd dev-mode

open localhost:3030

edit file.zd

reloaded automaticaly localhost:3030

git push

github actions
github pages

```


TODO:

* hickup keypathes - eval - search/filters <- niquola (stylization)
* search  <- apricotlace 
* emacs <- max
* zen integration + show errors (errors in menu) <- vlad


* Capitilize for headers (overide header)
* sytles - list styles
* table block
* backrefs - sytle 


* custom menu
* search
* devmode 

20:00 pm

Demo

* existing
* how to make knowledge base + how to extend (show on rpc)
