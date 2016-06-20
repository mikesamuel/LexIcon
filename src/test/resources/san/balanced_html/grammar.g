{
  ElType         <: (a, body, br, b, li, ol, p, table, tbody, td, tr, ul,
                     other_);
  CloseTagType   <: ElType;
  OuterElType    <: ElType;
  FlatStack      <: ElType*;
  PriorFlatStack <: ElType*;
  SelfClosers    <: ElType*;
}

start     := (@Elide [\ufeff])?
      @Scope{ElType, rec}   @Scope{FlatStack, rec}
      @Set  {ElType, body}  @Set  {FlatStack, ()}
      body;
text      := @Elide{:ElType <! (a, body, br, li, p, td)} "#text";
element   := "<" (
      @Scope{OuterElType, rec}
      @Set  {OuterElType, ElType}
      @Scope{ElType, rec}
      (
        tag_name
        elt_allowed
        ">"
        (
          void_body
        |
          @Scope{PriorFlatStack} @Set{PriorFlatStack, FlatStack}
          @Scope{FlatStack} (
            push_el body end_tag
          )
        )
      )
    );

tag_name   := (
      "a"     @Set{ElType, a}     ()
    | "body"  @Set{ElType, body}  ()
    | "br"    @Set{ElType, br}    ()
    | "b"     @Set{ElType, b}     ()
    | "li"    @Set{ElType, li}    ()
    | "ol"    @Set{ElType, ol}    ()
    | "p"     @Set{ElType, p}     ()
    | "table" @Set{ElType, table} ()
    | "tbody" @Set{ElType, tbody} ()
    | "td"    @Set{ElType, td}    ()
    | "tr"    @Set{ElType, tr}    ()
    | "ul"    @Set{ElType, ul}    ()
    | fail
    )
    break;

elt_allowed := !(open_tag_closes)
    @If{
        (OuterElType = a     & ElType <: (br, b))
      | (OuterElType = body  & ElType <: (a, br, b, ol, p, table, ul))
      | (OuterElType = b     & ElType <: (a, br))
      | (OuterElType = li    & ElType <: (a, br, b, ol, p, table, ul))
      | (OuterElType = ol    & ElType <: (li))
      // Strangely, <p> elements can contain tables but not lists.
      | (OuterElType = p     & ElType <: (a, br, b, table))
      | (OuterElType = table & ElType <: (tbody, tr))
      | (OuterElType = tbody & ElType <: (tr))
      | (OuterElType = td    & ElType <: (a, br, b, ol, p, table, ul))
      | (OuterElType = tr    & ElType <: (td))
      | (OuterElType = ul    & ElType <: (li))
    }();

open_tag_closes :=
    @Scope{SelfClosers}
    @Set{SelfClosers, FlatStack & ElType}
    @If{SelfClosers <: (a, p, li)} ();

orphan_end_tag :=
    "</"
    @Scope{CloseTagType} (
      @Scope{ElType} (
        ( tag_name   @Set{CloseTagType, ElType} ()
        | html_ident @Set{CloseTagType, other_} ()
        )
      )
      (
        // Element not on the stack.
        @If{
            (CloseTagType = a      & FlatStack <! (a))
          | (CloseTagType = b      & FlatStack <! (b))
          | (CloseTagType = br     & FlatStack <! (br))
          | (CloseTagType = li     & FlatStack <! (li))
          | (CloseTagType = ol     & FlatStack <! (ol))
          | (CloseTagType = p      & FlatStack <! (p))
          | (CloseTagType = table  & FlatStack <! (table))
          | (CloseTagType = tbody  & FlatStack <! (tbody))
          | (CloseTagType = td     & FlatStack <! (td))
          | (CloseTagType = tr     & FlatStack <! (tr))
          | (CloseTagType = ul     & FlatStack <! (ul))
          | (CloseTagType = body)
          | (CloseTagType = other_)
        }
        ()
      )
    )
    ">";

junk_tag := orphan_end_tag | junk_start_tag;

junk_start_tag :=
    "<" (@Scope{ElType} !(tag_name start_tag_allowed)) html_ident ">";

start_tag_allowed :=
    @If {
        ElType = a
      | ElType = b
      | ElType = br
      | ElType = li & FlatStack <: (ol, ul)
      | ElType = ol
      | ElType = p
      | ElType = table
      | ElType = td & FlatStack <: (tr)
      | ElType = tr & FlatStack <: (table, tbody)
      | ElType = ul
    } ();

html_ident := [a-z] [a-z0-9]*;

end_tag  :=
      "</" end_name ">"
    | @If{ElType = a}      @Implied{"</a>"}     ()
    | @If{ElType = b}      @Implied{"</b>"}     ()
    | @If{ElType = li}     @Implied{"</li>"}    ()
    | @If{ElType = ol}     @Implied{"</ol>"}    ()
    | @If{ElType = p}      @Implied{"</p>"}     ()
    | @If{ElType = table}  @Implied{"</table>"} ()
    | @If{ElType = tbody}  @Implied{"</tbody>"} ()
    | @If{ElType = td}     @Implied{"</td>"}    ()
    | @If{ElType = tr}     @Implied{"</tr>"}    ()
    | @If{ElType = ul}     @Implied{"</ul>"}    ()
    ;

end_name := (
      "a"     @If{ElType = a}     ()
    | "br"    @If{ElType = br}    ()
    | "b"     @If{ElType = b}     ()
    | "li"    @If{ElType = li}    ()
    | "ol"    @If{ElType = ol}    ()
    | "p"     @If{ElType = p}     ()
    | "table" @If{ElType = table} ()
    | "tbody" @If{ElType = tbody} ()
    | "td"    @If{ElType = td}    ()
    | "tr"    @If{ElType = tr}    ()
    | "ul"    @If{ElType = ul}    ()
    | fail
    )
    break;

push_el  :=
      @If{ElType = ol}    @Set{FlatStack, PriorFlatStack & ~li | ol} ()
    | @If{ElType = table} @Set{FlatStack, table}                     ()
    | @If{ElType = ul}    @Set{FlatStack, PriorFlatStack & ~li | ul} ()
    |                     @Set{FlatStack, PriorFlatStack | ElType}   ()
    | fail;

void_body := @If{ElType = br} () | fail;

break     := !([a-zA-Z0-9.:_\-]);

body      := (text | element | @Elide junk_tag)*;

fail      := [];
