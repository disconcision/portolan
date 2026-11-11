/* Shim for Hazel's src/util/WebUtil.re: what the canvas files use. */
open Virtual_dom.Vdom;

module Node = Node;
module Attr = Attr;

let clss = Attr.classes;
