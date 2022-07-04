" Vim syntax file
" Forked from https://github.com/rust-lang/rust.vim/blob/master/syntax/rust.vim
" TODO: start from scratch

if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

" Syntax definitions {{{1
" Basic keywords {{{2
syn keyword   largoConditional match if else
syn keyword   largoRepeat loop while
" `:syn match` must be used to prioritize highlighting `for` keyword.
syn match     largoRepeat /\<for\>/
" Highlight `for` keyword in `impl ... for ... {}` statement. This line must
" be put after previous `syn match` line to overwrite it.
syn match     largoKeyword /\%(\<impl\>.\+\)\@<=\<for\>/
syn keyword   largoRepeat in
syn keyword   largoTypedef type nextgroup=largoIdentifier skipwhite skipempty
syn keyword   largoStructure struct enum nextgroup=largoIdentifier skipwhite skipempty
syn keyword   largoUnion union nextgroup=largoIdentifier skipwhite skipempty contained
syn match largoUnionContextual /\<union\_s\+\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*/ transparent contains=largoUnion
syn keyword   largoOperator    as
syn keyword   largoExistential existential nextgroup=largoTypedef skipwhite skipempty contained
syn match largoExistentialContextual /\<existential\_s\+type/ transparent contains=largoExistential,largoTypedef

syn match     largoAssert      "\<assert\(\w\)*!" contained
syn match     largoPanic       "\<panic\(\w\)*!" contained
syn match     largoAsync       "\<async\%(\s\|\n\)\@="
syn keyword   largoKeyword     break
syn keyword   largoKeyword     box
syn keyword   largoKeyword     continue
syn keyword   largoKeyword     crate
syn keyword   largoKeyword     extern nextgroup=largoExternCrate,largoObsoleteExternMod skipwhite skipempty
syn keyword   largoKeyword     fn nextgroup=largoFuncName skipwhite skipempty
syn keyword   largoKeyword     impl let
syn keyword   largoKeyword     macro
syn keyword   largoKeyword     pub nextgroup=largoPubScope skipwhite skipempty
syn keyword   largoKeyword     return
syn keyword   largoKeyword     yield
syn keyword   largoSuper       super
syn keyword   largoKeyword     where
syn keyword   largoUnsafeKeyword unsafe
syn keyword   largoKeyword     use nextgroup=largoModPath skipwhite skipempty
" FIXME: Scoped impl's name is also fallen in this category
syn keyword   largoKeyword     mod trait nextgroup=largoIdentifier skipwhite skipempty
syn keyword   largoStorage     move mut ref static const
syn match     largoDefault     /\<default\ze\_s\+\(impl\|fn\|type\|const\)\>/
syn keyword   largoAwait       await
syn match     largoKeyword     /\<try\>!\@!/ display

syn keyword largoPubScopeCrate crate contained
syn match largoPubScopeDelim /[()]/ contained
syn match largoPubScope /([^()]*)/ contained contains=largoPubScopeDelim,largoPubScopeCrate,largoSuper,largoModPath,largoModPathSep,largoSelf transparent

syn keyword   largoExternCrate crate contained nextgroup=largoIdentifier,largoExternCrateString skipwhite skipempty
" This is to get the `bar` part of `extern crate "foo" as bar;` highlighting.
syn match   largoExternCrateString /".*"\_s*as/ contained nextgroup=largoIdentifier skipwhite transparent skipempty contains=largoString,largoOperator
syn keyword   largoObsoleteExternMod mod contained nextgroup=largoIdentifier skipwhite skipempty

syn match     largoIdentifier  contains=largoIdentifierPrime "\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display contained
syn match     largoFuncName    "\%(r#\)\=\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display contained

syn region largoMacroRepeat matchgroup=largoMacroRepeatDelimiters start="$(" end="),\=[*+]" contains=TOP
syn match largoMacroVariable "$\w\+"
syn match largoRawIdent "\<r#\h\w*" contains=NONE

" Reserved (but not yet used) keywords {{{2
syn keyword   largoReservedKeyword become do priv typeof unsized abstract virtual final override

" Built-in types {{{2
syn keyword   largoType        isize usize char bool u8 u16 u32 u64 u128 f32
syn keyword   largoType        f64 i8 i16 i32 i64 i128 str Self

" Things from the libstd v1 prelude (src/libstd/prelude/v1.rs) {{{2
" This section is just straight transformation of the contents of the prelude,
" to make it easy to update.

" Reexported core operators {{{3
syn keyword   largoTrait       Copy Send Sized Sync
syn keyword   largoTrait       Drop Fn FnMut FnOnce

" Reexported functions {{{3
" There’s no point in highlighting these; when one writes drop( or drop::< it
" gets the same highlighting anyway, and if someone writes `let drop = …;` we
" don’t really want *that* drop to be highlighted.
"syn keyword largoFunction drop

" Reexported types and traits {{{3
syn keyword largoTrait Box
syn keyword largoTrait ToOwned
syn keyword largoTrait Clone
syn keyword largoTrait PartialEq PartialOrd Eq Ord
syn keyword largoTrait AsRef AsMut Into From
syn keyword largoTrait Default
syn keyword largoTrait Iterator Extend IntoIterator
syn keyword largoTrait DoubleEndedIterator ExactSizeIterator
syn keyword largoEnum Option
syn keyword largoEnumVariant Some None
syn keyword largoEnum Result
syn keyword largoEnumVariant Ok Err
syn keyword largoTrait SliceConcatExt
syn keyword largoTrait String ToString
syn keyword largoTrait Vec

" Other syntax {{{2
syn keyword   largoSelf        self
syn keyword   largoBoolean     true false

" If foo::bar changes to foo.bar, change this ("::" to "\.").
" If foo::bar changes to Foo::bar, change this (first "\w" to "\u").
syn match     largoModPath     "\w\(\w\)*::[^<]"he=e-3,me=e-3
syn match     largoModPathSep  "::"

syn match     largoFuncCall    "\w\(\w\)*("he=e-1,me=e-1
syn match     largoFuncCall    "\w\(\w\)*::<"he=e-3,me=e-3 " foo::<T>();

" This is merely a convention; note also the use of [A-Z], restricting it to
" latin identifiers rather than the full Unicode uppercase. I have not used
" [:upper:] as it depends upon 'noignorecase'
"syn match     largoCapsIdent    display "[A-Z]\w\(\w\)*"

syn match     largoOperator     display "\%(+\|-\|/\|*\|=\|\^\|&\||\|!\|>\|<\|%\)=\?"
" This one isn't *quite* right, as we could have binary-& with a reference
syn match     largoSigil        display /&\s\+[&~@*][^)= \t\r\n]/he=e-1,me=e-1
syn match     largoSigil        display /[&~@*][^)= \t\r\n]/he=e-1,me=e-1
" This isn't actually correct; a closure with no arguments can be `|| { }`.
" Last, because the & in && isn't a sigil
syn match     largoOperator     display "&&\|||"
" This is largoArrowCharacter rather than largoArrow for the sake of matchparen,
" so it skips the ->; see http://stackoverflow.com/a/30309949 for details.
syn match     largoArrowCharacter display "->"
syn match     largoQuestionMark display "?\([a-zA-Z]\+\)\@!"

syn match     largoMacro       '\w\(\w\)*!' contains=largoAssert,largoPanic
syn match     largoMacro       '#\w\(\w\)*' contains=largoAssert,largoPanic

syn match     largoEscapeError   display contained /\\./
syn match     largoEscape        display contained /\\\([nrt0\\'"]\|x\x\{2}\)/
syn match     largoEscapeUnicode display contained /\\u{\%(\x_*\)\{1,6}}/
syn match     largoStringContinuation display contained /\\\n\s*/
syn region    largoString      matchgroup=largoStringDelimiter start=+b"+ skip=+\\\\\|\\"+ end=+"+ contains=largoEscape,largoEscapeError,largoStringContinuation
syn region    largoString      matchgroup=largoStringDelimiter start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=largoEscape,largoEscapeUnicode,largoEscapeError,largoStringContinuation,@Spell
syn region    largoString      matchgroup=largoStringDelimiter start='b\?r\z(#*\)"' end='"\z1' contains=@Spell

" Match attributes with either arbitrary syntax or special highlighting for
" derives. We still highlight strings and comments inside of the attribute.
syn region    largoAttribute   start="#!\?\[" end="\]" contains=@largoAttributeContents,largoAttributeParenthesizedParens,largoAttributeParenthesizedCurly,largoAttributeParenthesizedBrackets,largoDerive
syn region    largoAttributeParenthesizedParens matchgroup=largoAttribute start="\w\%(\w\)*("rs=e end=")"re=s transparent contained contains=largoAttributeBalancedParens,@largoAttributeContents
syn region    largoAttributeParenthesizedCurly matchgroup=largoAttribute start="\w\%(\w\)*{"rs=e end="}"re=s transparent contained contains=largoAttributeBalancedCurly,@largoAttributeContents
syn region    largoAttributeParenthesizedBrackets matchgroup=largoAttribute start="\w\%(\w\)*\["rs=e end="\]"re=s transparent contained contains=largoAttributeBalancedBrackets,@largoAttributeContents
syn region    largoAttributeBalancedParens matchgroup=largoAttribute start="("rs=e end=")"re=s transparent contained contains=largoAttributeBalancedParens,@largoAttributeContents
syn region    largoAttributeBalancedCurly matchgroup=largoAttribute start="{"rs=e end="}"re=s transparent contained contains=largoAttributeBalancedCurly,@largoAttributeContents
syn region    largoAttributeBalancedBrackets matchgroup=largoAttribute start="\["rs=e end="\]"re=s transparent contained contains=largoAttributeBalancedBrackets,@largoAttributeContents
syn cluster   largoAttributeContents contains=largoString,largoCommentLine,largoCommentBlock,largoCommentLineDocError,largoCommentBlockDocError
syn region    largoDerive      start="derive(" end=")" contained contains=largoDeriveTrait
" This list comes from src/libsyntax/ext/deriving/mod.rs
" Some are deprecated (Encodable, Decodable) or to be removed after a new snapshot (Show).
syn keyword   largoDeriveTrait contained Clone Hash largocEncodable largocDecodable Encodable Decodable PartialEq Eq PartialOrd Ord Rand Show Debug Default FromPrimitive Send Sync Copy

" dyn keyword: It's only a keyword when used inside a type expression, so
" we make effort here to highlight it only when largo identifiers follow it
" (not minding the case of pre-2018 largo where a path starting with :: can
" follow).
"
" This is so that uses of dyn variable names such as in 'let &dyn = &2'
" and 'let dyn = 2' will not get highlighted as a keyword.
syn match     largoKeyword "\<dyn\ze\_s\+\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)" contains=largoDynKeyword
syn keyword   largoDynKeyword  dyn contained

" Number literals
syn match     largoDecNumber   display "\<[0-9][0-9_]*\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match     largoHexNumber   display "\<0x[a-fA-F0-9_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match     largoOctNumber   display "\<0o[0-7_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match     largoBinNumber   display "\<0b[01_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="

" Special case for numbers of the form "1." which are float literals, unless followed by
" an identifier, which makes them integer literals with a method call or field access,
" or by another ".", which makes them integer literals followed by the ".." token.
" (This must go first so the others take precedence.)
syn match     largoFloat       display "\<[0-9][0-9_]*\.\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\|\.\)\@!"
" To mark a number as a normal float, it must have at least one of the three things integral values don't have:
" a decimal point and more numbers; an exponent; and a type suffix.
syn match     largoFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\%([eE][+-]\=[0-9_]\+\)\=\(f32\|f64\)\="
syn match     largoFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\(f32\|f64\)\="
syn match     largoFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\=\(f32\|f64\)"

" For the benefit of delimitMate
syn region largoLifetimeCandidate display start=/&'\%(\([^'\\]\|\\\(['nrt0\\\"]\|x\x\{2}\|u{\%(\x_*\)\{1,6}}\)\)'\)\@!/ end=/[[:cntrl:][:space:][:punct:]]\@=\|$/ contains=largoSigil,largoLifetime
syn region largoGenericRegion display start=/<\%('\|[^[:cntrl:][:space:][:punct:]]\)\@=')\S\@=/ end=/>/ contains=largoGenericLifetimeCandidate
syn region largoGenericLifetimeCandidate display start=/\%(<\|,\s*\)\@<='/ end=/[[:cntrl:][:space:][:punct:]]\@=\|$/ contains=largoSigil,largoLifetime

"largoLifetime must appear before largoCharacter, or chars will get the lifetime highlighting
syn match     largoLifetime    display "\'\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*"
syn match     largoLabel       display "\'\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*:"
syn match     largoLabel       display "\%(\<\%(break\|continue\)\s*\)\@<=\'\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*"
syn match   largoCharacterInvalid   display contained /b\?'\zs[\n\r\t']\ze'/
" The groups negated here add up to 0-255 but nothing else (they do not seem to go beyond ASCII).
syn match   largoCharacterInvalidUnicode   display contained /b'\zs[^[:cntrl:][:graph:][:alnum:][:space:]]\ze'/
syn match   largoCharacter   /b'\([^\\]\|\\\(.\|x\x\{2}\)\)'/ contains=largoEscape,largoEscapeError,largoCharacterInvalid,largoCharacterInvalidUnicode
syn match   largoCharacter   /'\([^\\]\|\\\(.\|x\x\{2}\|u{\%(\x_*\)\{1,6}}\)\)'/ contains=largoEscape,largoEscapeUnicode,largoEscapeError,largoCharacterInvalid

syn match largoShebang /\%^#![^[].*/
syn region largoCommentLine                                                  start="//"                      end="$"   contains=largoTodo,@Spell
syn region largoCommentLineDoc                                               start="//\%(//\@!\|!\)"         end="$"   contains=largoTodo,@Spell
syn region largoCommentLineDocError                                          start="//\%(//\@!\|!\)"         end="$"   contains=largoTodo,@Spell contained
syn region largoCommentBlock             matchgroup=largoCommentBlock         start="/\*\%(!\|\*[*/]\@!\)\@!" end="\*/" contains=largoTodo,largoCommentBlockNest,@Spell
syn region largoCommentBlockDoc          matchgroup=largoCommentBlockDoc      start="/\*\%(!\|\*[*/]\@!\)"    end="\*/" contains=largoTodo,largoCommentBlockDocNest,largoCommentBlockDoclargoCode,@Spell
syn region largoCommentBlockDocError     matchgroup=largoCommentBlockDocError start="/\*\%(!\|\*[*/]\@!\)"    end="\*/" contains=largoTodo,largoCommentBlockDocNestError,@Spell contained
syn region largoCommentBlockNest         matchgroup=largoCommentBlock         start="/\*"                     end="\*/" contains=largoTodo,largoCommentBlockNest,@Spell contained transparent
syn region largoCommentBlockDocNest      matchgroup=largoCommentBlockDoc      start="/\*"                     end="\*/" contains=largoTodo,largoCommentBlockDocNest,@Spell contained transparent
syn region largoCommentBlockDocNestError matchgroup=largoCommentBlockDocError start="/\*"                     end="\*/" contains=largoTodo,largoCommentBlockDocNestError,@Spell contained transparent

" FIXME: this is a really ugly and not fully correct implementation. Most
" importantly, a case like ``/* */*`` should have the final ``*`` not being in
" a comment, but in practice at present it leaves comments open two levels
" deep. But as long as you stay away from that particular case, I *believe*
" the highlighting is correct. Due to the way Vim's syntax engine works
" (greedy for start matches, unlike largo's tokeniser which is searching for
" the earliest-starting match, start or end), I believe this cannot be solved.
" Oh you who would fix it, don't bother with things like duplicating the Block
" rules and putting ``\*\@<!`` at the start of them; it makes it worse, as
" then you must deal with cases like ``/*/**/*/``. And don't try making it
" worse with ``\%(/\@<!\*\)\@<!``, either...

syn keyword largoTodo contained TODO FIXME XXX NB NOTE SAFETY

" asm! macro {{{2
syn region largoAsmMacro matchgroup=largoMacro start="\<asm!\s*(" end=")" contains=largoAsmDirSpec,largoAsmSym,largoAsmConst,largoAsmOptionsGroup,largoComment.*,largoString.*

" Clobbered registers
syn keyword largoAsmDirSpec in out lateout inout inlateout contained nextgroup=largoAsmReg skipwhite skipempty
syn region  largoAsmReg start="(" end=")" contained contains=largoString

" Symbol operands
syn keyword largoAsmSym sym contained nextgroup=largoAsmSymPath skipwhite skipempty
syn region  largoAsmSymPath start="\S" end=",\|)"me=s-1 contained contains=largoComment.*,largoIdentifier

" Const
syn region  largoAsmConstBalancedParens start="("ms=s+1 end=")" contained contains=@largoAsmConstExpr
syn cluster largoAsmConstExpr contains=largoComment.*,largo.*Number,largoString,largoAsmConstBalancedParens
syn region  largoAsmConst start="const" end=",\|)"me=s-1 contained contains=largoStorage,@largoAsmConstExpr

" Options
syn region  largoAsmOptionsGroup start="options\s*(" end=")" contained contains=largoAsmOptions,largoAsmOptionsKey
syn keyword largoAsmOptionsKey options contained
syn keyword largoAsmOptions pure nomem readonly preserves_flags noreturn nostack att_syntax contained

" Folding rules {{{2
" Trivial folding rules to begin with.
" FIXME: use the AST to make really good folding
syn region largoFoldBraces start="{" end="}" transparent fold

if !exists("b:current_syntax_embed")
    let b:current_syntax_embed = 1
    syntax include @largoCodeInComment <sfile>:p:h/largo.vim
    unlet b:current_syntax_embed

    " Currently regions marked as ```<some-other-syntax> will not get
    " highlighted at all. In the future, we can do as vim-markdown does and
    " highlight with the other syntax. But for now, let's make sure we find
    " the closing block marker, because the rules below won't catch it.
    syn region largoCommentLinesDocNonlargoCode matchgroup=largoCommentDocCodeFence start='^\z(\s*//[!/]\s*```\).\+$' end='^\z1$' keepend contains=largoCommentLineDoc

    " We borrow the rules from largo’s src/liblargodoc/html/markdown.rs, so that
    " we only highlight as largo what it would perceive as largo (almost; it’s
    " possible to trick it if you try hard, and indented code blocks aren’t
    " supported because Markdown is a menace to parse and only mad dogs and
    " Englishmen would try to handle that case correctly in this syntax file).
    syn region largoCommentLinesDoclargoCode matchgroup=largoCommentDocCodeFence start='^\z(\s*//[!/]\s*```\)[^A-Za-z0-9_-]*\%(\%(should_panic\|no_run\|ignore\|allow_fail\|largo\|test_harness\|compile_fail\|E\d\{4}\|edition201[58]\)\%([^A-Za-z0-9_-]\+\|$\)\)*$' end='^\z1$' keepend contains=@largoCodeInComment,largoCommentLineDocLeader
    syn region largoCommentBlockDoclargoCode matchgroup=largoCommentDocCodeFence start='^\z(\%(\s*\*\)\?\s*```\)[^A-Za-z0-9_-]*\%(\%(should_panic\|no_run\|ignore\|allow_fail\|largo\|test_harness\|compile_fail\|E\d\{4}\|edition201[58]\)\%([^A-Za-z0-9_-]\+\|$\)\)*$' end='^\z1$' keepend contains=@largoCodeInComment,largoCommentBlockDocStar
    " Strictly, this may or may not be correct; this code, for example, would
    " mishighlight:
    "
    "     /**
    "     ```largo
    "     println!("{}", 1
    "     * 1);
    "     ```
    "     */
    "
    " … but I don’t care. Balance of probability, and all that.
    syn match largoCommentBlockDocStar /^\s*\*\s\?/ contained
    syn match largoCommentLineDocLeader "^\s*//\%(//\@!\|!\)" contained
endif

" Default highlighting {{{1
hi def link largoDecNumber       largoNumber
hi def link largoHexNumber       largoNumber
hi def link largoOctNumber       largoNumber
hi def link largoBinNumber       largoNumber
hi def link largoIdentifierPrime largoIdentifier
hi def link largoTrait           largoType
hi def link largoDeriveTrait     largoTrait

hi def link largoMacroRepeatDelimiters   Macro
hi def link largoMacroVariable Define
hi def link largoSigil         StorageClass
hi def link largoEscape        Special
hi def link largoEscapeUnicode largoEscape
hi def link largoEscapeError   Error
hi def link largoStringContinuation Special
hi def link largoString        String
hi def link largoStringDelimiter String
hi def link largoCharacterInvalid Error
hi def link largoCharacterInvalidUnicode largoCharacterInvalid
hi def link largoCharacter     Character
hi def link largoNumber        Number
hi def link largoBoolean       Boolean
hi def link largoEnum          largoType
hi def link largoEnumVariant   largoConstant
hi def link largoConstant      Constant
hi def link largoSelf          Constant
hi def link largoFloat         Float
hi def link largoArrowCharacter largoOperator
hi def link largoOperator      Operator
hi def link largoKeyword       Keyword
hi def link largoDynKeyword    largoKeyword
hi def link largoTypedef       Keyword " More precise is Typedef, but it doesn't feel right for largo
hi def link largoStructure     Keyword " More precise is Structure
hi def link largoUnion         largoStructure
hi def link largoExistential   largoKeyword
hi def link largoPubScopeDelim Delimiter
hi def link largoPubScopeCrate largoKeyword
hi def link largoSuper         largoKeyword
hi def link largoUnsafeKeyword Exception
hi def link largoReservedKeyword Error
hi def link largoRepeat        Conditional
hi def link largoConditional   Conditional
hi def link largoIdentifier    Identifier
hi def link largoCapsIdent     largoIdentifier
hi def link largoModPath       Include
hi def link largoModPathSep    Delimiter
hi def link largoFunction      Function
hi def link largoFuncName      Function
hi def link largoFuncCall      Function
hi def link largoShebang       Comment
hi def link largoCommentLine   Comment
hi def link largoCommentLineDoc SpecialComment
hi def link largoCommentLineDocLeader largoCommentLineDoc
hi def link largoCommentLineDocError Error
hi def link largoCommentBlock  largoCommentLine
hi def link largoCommentBlockDoc largoCommentLineDoc
hi def link largoCommentBlockDocStar largoCommentBlockDoc
hi def link largoCommentBlockDocError Error
hi def link largoCommentDocCodeFence largoCommentLineDoc
hi def link largoAssert        PreCondit
hi def link largoPanic         PreCondit
hi def link largoMacro         Macro
hi def link largoType          Type
hi def link largoTodo          Todo
hi def link largoAttribute     PreProc
hi def link largoDerive        PreProc
hi def link largoDefault       StorageClass
hi def link largoStorage       StorageClass
hi def link largoObsoleteStorage Error
hi def link largoLifetime      Special
hi def link largoLabel         Label
hi def link largoExternCrate   largoKeyword
hi def link largoObsoleteExternMod Error
hi def link largoQuestionMark  Special
hi def link largoAsync         largoKeyword
hi def link largoAwait         largoKeyword
hi def link largoAsmDirSpec    largoKeyword
hi def link largoAsmSym        largoKeyword
hi def link largoAsmOptions    largoKeyword
hi def link largoAsmOptionsKey largoAttribute

" Other Suggestions:
" hi largoAttribute ctermfg=cyan
" hi largoDerive ctermfg=cyan
" hi largoAssert ctermfg=yellow
" hi largoPanic ctermfg=red
" hi largoMacro ctermfg=magenta

syn sync minlines=200
syn sync maxlines=500

let b:current_syntax = "largo"

" vim: set et sw=4 sts=4 ts=8:
