#!/usr/local/bin/sbcl --script

;;2021-12-10T24:00:00ZUTC+8

(declaim (sb-ext:muffle-conditions cl:warning))

(load "~/quicklisp/setup.lisp")

(ql:quickload :split-sequence)

(setf the-input-string 
"[{(<<{(([<<{<<[]()>[<>[]]>[[<>[]]{{}{}}]}<<[<>()][{}]><[<>{}]<<>()>>>>([<([]<>)([][])>(<<>[]>([]()))]<[
{([[[{{([{{{([[]()][()<>])}[<<(){}><<>()>>{{[]<>}[(){}]}]}<<(<<>[]>{[][]}){<<>()>({}()}}><{
{[[([{([<(<{{[{}{}]<[]()>}(<<>[]><<>()>)}(<[<><>](<>[])><<(){}>{(){}}>)>{{[({}[]){[][]}]({()<>}<[]<>>)}})>({(
{[<([(<<{<<<[<{}{}>({}[])][<[]{}>{[]{}}]>{[{[][]}[{}<>]]{(<><>){{}[]}}}>([<[<>{}]([][])>{(<>{})}][{({
<{{[{[<(<[{[(<()<>>([][]))[[{}()]]]}<{<{{}{}}((){})>((()())([]{}))}<((<>{}))>>]<{[[<()[]>]{(<>[])
<((({<((<([<[<[][]>[[]{}]]<{[][]}({})>>{<<()>({}())>[(<>)<[]()>]}]{[{<<>[]>{{}<>}}<<()<>>([][])>]})[({{((
[{<[{<({({[(<{{}[]}[[]()]>{<()()><{}()>})({[{}<>](()())}({[]{}}({}{}))>]([[[<>[]]<()[]>]]([[<><>]({}
{(<[<<{([([(<[{}[]]<{}{}>><<()[]>([]<>}>)]{[[{()}({}())]<[<>[]]([]<>)>](<[[]{}](<><>)><[[]{}]{<>[]}>)})]
({([{[({{<([[([]())]{[[]{}]}]{<{(){}}<{}{}>><([]{})>})[(<{<>()}>{(<><>)[[]{}]}){{{{}<>}({}<>)}[<()
<[<[{((<<{[((<[]<>>[[]()]){{()[]](<>)})(({[][]}[[][]])[(<><>)<{}>])][<[{[]()}{[]()}][{<>{}}({}<>)
<({(<(<{[[({({()[]}<{}<>>)[<{}[]>{{}()}]}([[{}](()[])][{[]{}}(()[])])]{[[{()[]}]<[[][]][{}]>]}]<(<<<[]()>[
[<<<{((<{(<([[{}{}]{()<>}]{{{}<>}[<>()]}){[[[]()][[][]]]{([]())[()<>]}}>{({[<>()]}([[][]]<(){}>))<<<{}<
[[{(([(<{(<({[()()]([]{})}[({}<>)])([<[]{}><()<>>][<<>[]><<><>>])>)[[{[{()[]}{[]}]}]]}>)]{<{{(<[{[[]<>]<()[]>
<{(<[{{([{{[(<[]<>><()<>>){{{}{}}}]}(([[<><>]([][])]<{()}{{}()}>)<{[()<>][<>]}<(<>[])>>)}])
(<{({([<([<{<(<>()){()()}>([<>()]({}()))}><(([{}{}]{<>{}})<<[]()><[]{}>})<{([]<>)<[]()>}({{
[{([(({[(<[[{{{}<>}({}())}{{(){}}<<>>}]<<({}())(<><>)]<<<><>><()()>>>]({(<{}<>>{()[]})}<<{{}{}}({})><<<>(
(<[(({<(<{({{<()<>>{{}()}}}[[{()[]}{[]{}}]<({}())([]{})>])}{<{<(()[])([]<>)><<<>{}><{}[]>>}<[[<><>](()[]}]>>{
({({<({({(<{({{}{}}(()[]))(<{}{}>[{}()])}((<{}<>><()[]>)[(()<>)[{}<>]])>)}[[<<{<[]{}>{<>[]}}{{(){
({<<([(<<<(<{(()())[{}[]]}(<[]<>>[{}{}])>)<(<[{}[]]{[]{}}>({[][]}[[][]])>>><[{([[]()]{{}()
[({{[[(({[<[([{}[]])<<(){}>(()<>)>]({<[][]>[{}<>]}[<[]<>>])>({(<<>[]>{{}{}}){<[]{}><{}<>>}}([({}<
((<[[{({<<<([({}{})[[]<>]]{(()<>)[[]{}]})({{<>}[<>[]]}[[<>[]]<[][]>])>>>}[[<<{(<<>()>(()()))<{<>{}}(()
{{{[[[([{<(<[([]())[[]()]]<(()())([]{})>>([<[][]>]{({}()){()<>}}))><[([[<>[]][[]<>]][<()[]><{}{
[<{[(<<{({<({{()[]}[{}<>]}{({}<>)[(){}]})[<([]{})><([]<>){()()}>]>})}>[[<{[<<[<>()]<()<>>>{[{}{}]<<>[]>
<((((((<{(<([[<><>][<>[]]]{<{}<>>})>)(([<[()()]>[(<>[])<[]{}>]][<([]{})([]{})>{{[]}({}())}]){[[[()[
<([{((({{[({[[()]][{[]}{<>[]}]}<{<(){}>[[][]]}<([]())<[][]>>>)<<{<()()>({}())}{<()()>}>{{[{}()]}}>]}
<[({<[(<<<<{[[<>{}]([]{})]}>>>[<[[{(<>())([][]}}(<[]<>>)]<<{<>()}{[]<>}>{<()>([]())}>]<<[({}{})<[
[[([({<({{(<(<()()>(()<>))>[{<<>()>([]<>)}{{<><>}{{}[]>}]){([[[]()]<[]<>>](([]())<[][]>))}}}{<{<{<<>()
<<{[[<(({(({<{[][]}(<>{})>{(<>())}}{<[[]<>]<{}()>){{[]<>}[()]}}))}))>]]{{<<{{<<<{([][])<{}()>}[[<>()][()[]
{<{[[(<[((([<{[][]}[{}]>]((([][])([]()))([<><>](<>{}))))){(<[[(){}]<[][]>]>{((()()}({}[]))<[<>{}]{<><>}>
([[[[[([<{({{<[]<>><{}<>>}{{<>()}[[]<>]}}(<[<><>]{()()}>))<[{({}<>){(){}}}<({}())[()[]]>]{([()<>][[][]
({([(({<<{{[(({}[])([][]))[({}{})]]}([<<[]<>>([]{})>])]>>[{[[(<<()()><<>{}>>)(([[][]](<>[]))
{({[<({[({[([[(){}]{(){}}])]<{<<(){}>[{}[]]><{[]{}}[[]<>]>}{{{<>{}}[[][]]}[[[]<>]]}>}([{((<>[])<[][]>)}]
({([<(((({<[<<[]<>>{{}[]}>((<>{})<[]{}>)]>{(<{{}[]}({}{})><<()<>>(<>[])>)[{(())([]())}]}})))[{<(({{{[]{}}[<
<<([{{[[<<(<[{[][]}<{}{}>]<[{}{}]<<>{}>>><[[{}{}][{}]]<<{}<>>[{}()]>>)(<[(<>{})<()[]>]{(()())}><[[[]]<{}>][(<
{[[([<({[{[<[([]<>)<[]{}>][(<>())[[]<>]]>[([<>[]](<>))([()()]({}()))]]{{(<()[]>{{}[]>)[{<>}(()())]}}}{<{{
([[[(([{{<[{<[<>[]]<<>[]>><(()())<{}[]>>}(([[][]]<{}<>>))]<[[(<><>}{()()}][[{}[]][()[]]]](([{}{}]<[][]>)<[{}
{({{(<([[{({{<[]{}>[[][]]}{{<>()}<[]{}>}})({({[]})[<()()>{[]<>}]})}<{[[<{}>({}())]{{[][]}<[]()>>]([{(){
{([((<<{[{{[<({}())[()[]]>(<{}<>>({}()))]}}<{<([[][]]([]{})){(<>[]){()[]}}><[({})[<>[]]][(
{(((<[([({(([{<>}<[]{}>]))}({{([<>{}](<>{})){(<>{}){[]{}}}}[[[<>[]]]<{()<>}{<><>})]}({({[][]}<[]{}>)[{[]{}}]}
[{(({<[{(([{<[()[]]<[][]>>([{}<>][()()])}([<<>[]>[[]{}]][<{}[]>({}())])]<({({}[])}[{()()}[{}[]
[[{{<<<[[{<<[<[]{}><()[]>]{[[][]]{<><>}}>>{({[[]{}][<><>]}(([]<>)<<>()>))(([{}<>]<[][]>)<([]<>)((
[{{(({[(({[[<[(){}]>(<{}<>>)]<(([]<>)<<>()>)[<[]{}>{<>()}]>]({<[[]{}]{<><>>>}<([<>()][[]<>]
[{<<{{(((<[[{(<>{}){[]<>}}((()[])[()<>])]<[[()]](<<>[]>[(){}])>](<({{}[]}<()<>>}<[[][]]<{}<>
<[(<{[[[{[{[[[[]<>)({}())]([()()][(){}])][<{[]()}[(){}]>[[(){}](<>())]]}<[{[()<>]{[]<>}}{<()()>}](
{[<<[{{([{<[([{}()]([][]))<([]())<<><>>>]<(([]<>)[{}]){<{}()>({}<>)}>>{<<<<><>>(()[])><{{}{}}<[][]>>>{([{}[
((({[<<<(<[<<{<>{}})[<{}<>>]>{[({}<>){[]}](([]<>)<(){}>)}]>{{<{<{}{}>[[]{}]}({[][]}<()[]>)><{{()}{{}}}[{{}{}}
{(([{[({[([[<([][])(()<>)>(<(){}>[<>[]])]<{[<>[]]({}[])}(<[]{}>{{}{}})>])<<({[()[]]((){})}{{{}{
([(<{(<<((([<{[]}>[[[][]](<><>)]][<([]){<>{}}>{[<>{}]}])(<[{<>{}}({}<>)]([()[]]([]<>))>))<{<([()
<<{{({<{{{<<({(){}}<[]{}>)><([(){}]({}[]))[[{}()]({}())]>>{<<[{}[]]{()<>}><[[]<>][()<>]>>[{
[[[{{{[{[(<(([()[]]<(){}>))[(([][])[<>])<([][]){<>()}>]>{[<<()[]><{}[]>>((<>)[(){}]))})(<<[{()()}<()
<{[{((<({<<[<<{}()>[(){}]>[(()()){<>()}]](<[{}<>][{}()]>{<(){}>})>>[<<[<(){}>[<>{}]]>((<()()>{<>
{((<[<([(({[<<{}<>>{<><>}>[<()()><{}[]>]][{[[][]]{()()}}[[<>]{{}()}]]}<(<(<>{})<()<>>>{{()
{(<[(<([{<<<({{}()}({}<>))[<[]><[]<>>]>{<<{}<>>[[]()>>}>[{<<{}()>[[]{}]>}{{{()<>}}}]>}]){(<[({<<<><>><[][]>>}
<[<[(([{<[<{[{[][]}{<>[]}])>][([[[[]{}][{}<>]][[{}{}]]]){(({<>[]}[<>[]]){{[]{}}{()()}})<(<[]<>><[][]>)>}]>}[
{(<[[[(<([[{{[()[]]{<>()}}{<[]{}>{<>[]}}}(<([]<>)([]())><<()[]>(<>{})>)]]([<([<>](())>{{<><>}<(){}>}>][<(
<<[([[{<[[[[{([]())<<>[]>}]({{{}{}}({}[])}<{()[]}<()<>>>)]{{<({}())[<>[]]><{<>}(()<>)>}([({}{})[<>]
{([({[(({{<[[{{}()>[<>]]({<>[]}(()()))]([[()<>]([]())]{[()[]](<><>)})>{[<<()()><(){}>>[[()()][
{[[{<{([(([[{({}[]){()()}}{(<>{})[[]<>]}]<[[{}<>]<[][]>]((()[]))>])){(<<([[]{}]([]{})){<{}<>><()()>}>{<{[]
<{({(({{{([<{[()[]]<()<>>}{({}())[{}{}]}><(([]())<<>[]>)<{[]()}[(){}]>>)({(<()>)([{}{}][()<>])}[<[[]<>][()()]
[{{(((([{[{(<(<>())([]{})>[([][])({}[])])<{<(){}>[[][]]}[<[]()>{[][]}]>}((<([]()}{[]{}}>){<({})><{(){}}>})
{(<{<((((<(<<<[]<>>(<>())>{[{}[])<[]{}>}>)><[[([{}<>]{{}<>})<{()()}{<>{}}>]{[<()()>{()[]}][[<
{(<<[<((<<<<{{<>{}}{<>{}}}[(()())<<>()>]>[[([]())<{}()>]((<><>)<<>[]>)]>{([<()()>[{}]]{[<>[]]<<>()>})<<({}<
<{[{(((<({<<((()<>){[][]})[{[]<>}[[]<>]]>>})>))[<{{{[<{[{}{}]<[]()>}<{()[]>>>[<<{}{}>{<><>}>[[<>{}][{}<
[[{<{<[[<{(([<[]<>>{[]()}]){[<[]<>>[{}]](<(){}>[[][]])})}>]][[<(<<[[[][]][[]()]]<[[]<>]>>>{[({{}()}<{}<>>)]<(
{{((([<((<<[[{()}(()[])]][([(){}]<[]<>>)]>(([[{}()][()[]]](((){}){(){}}])({[<><>]<{}{}>}(([][])<<>{}>)))>)((
<{([[(<{[<[[[([]())<<>()>][<{}{}>{{}{}}>]{[({}{})({}<>)][(<>{})<{}()>]}]([{{{}()}{[][]}}])>([
<[<{{<<{{[[(([(){}]({}[]])({<><>}<[]{}>))]{<(({}())){<[][]>}>{{({}<>)[[]<>]}(<<>[]>[[]<>])}}]}}>>[{
(<<<{({[[[[<{(<><>){()()}}>{[<<>{}>([]{})][[{}<>][[]{}]]}]<{((<>{})[()<>])(({}()){{}()})}>]]{{({[((){})](<[]
<[{<{{[<[{{([(<>())[(){}]]<[<>()]<()<>>>)}}][<[(([()<>]({}())){(()[])<{}()>])<[(<>[])({}())]>]{((
<{{[{{([<{([((<>{})({}{}))](<({}[]){()<>}>(({}())[[]()]))){([[<>{}]{()<>}])}}<(<[<()[]><()<>>]}<({[]<
{({[[({<{{[[([()])<{()[]}{<>[]}>]]}}>[((<[<([]())({}())>]<<{<>[]}[{}<>]>>>{[[([]{})([])}{<<><>>{<>()}
<<{{<(({[<<(((<><>){{}[]})(<{}[]>))[<<[]{}>([]{})><<[]<>>>]>{{(([]()){<>{}}){{<>[]}((){})}
{<{[({{<{[(([<(){}><[]<>>})[(<()[]>({}<>))({[]{}}[[]<>])])][<{<<[]()>{{}[]}>}{(([]){{}<>}){<{}{}><{}
[[{([<[<[{<([(()<>)}[[{}{}]<[]()>])[[<()[]>]([{}{}][()<>])]>([[<<>{}>]{{[][]}[(){}]}][{[()()](<><>)}])}{<<<
[[[{[<<<<<[(([<><>])<<{}[]><{}()>>)[[<<>[]>[()[]]]{(()<>){<>{}}}]]<<({[]{}}[[]()]){(()<>)[[]{}]}>(({{}{
[[(([{{<([{[[<()()>({}[])]{<<>[]>((){})}]({{{}}<{}>}((()())({}[])))}(<[{[]}([]<>)]>{{<()<>>(<>{})}
(((({[(<{(<{<[()()][{}<>]>}{<<{}<>>[()[]]>}><<{{()[]}[[][]]}>>)}>>]{([[([({[{}()][<>{}]}){<[{}()]<{}
{[[[{{[[<<{(({[]<>}){{[][]}<<><>>})[[[(){}]<<>{}>]{<<>[]>{{}()}}]}>>]](([([{<[(){}][{}()]>{<[]<>><
<[[[{<<<{([{(<()()>)<[<>[]]({}())>}{{([]{}){{}}}[[(){}]<<>[]>]}](([<{}()>](({}[])[[]()]))<[[{}()]<{}[]>]>)
(({(({{<{{<((({}()))[<()()>[<>{}]])>}{{{[[{}<>][{}[]]]{<<><>><<>[]>}}}}}>}}[({{({[<[[]()][
((({[<{[((<[[<<>{}>}{{<>[]}}]>){{({{<>{}}}{{()()}(<>[])})({<[]()>[()()]}{<[]()>{[]{}}})}<[(<()()>{{}
([{{(<<<<[([[{{}{}}<{}<>>]<({}{}){()}>]{{{()<>}[[]{}]}[{[]<>}{<><>}]})][{[[(<><>)[{}<>]][([](
{<<<[[({<{[({(<><>)[<><>]}{[(){}]{<>[]}})]{<([<>{}]{()[]})((<>()))>}}>[[<[{<{}()>}(({}<>)[{}[]])]<{(<>[]
[({(<[[<[<({({<>[]}{(){}}){([]()>(()[])}})>[[{(<()[]>[<>()])}{[[<>()]({}[])]{(()[])<()<>>}}]]]([{{<<()[]
{[([(([[{<<<{<[]>{<><>}}{{[]}(()())}><[{<>()}[<>[]]]([<><>]([]{}))>>{(({<><>}))({{{}[]}<<>[]>}[{{}()}])}
[([<[{([<<<(<{()<>}<[]()>>{[<><>][<><>]})([{{}}]{{{}[]}(<><>)})>>(({{({}[])<[]>}[(<>())(<><
[{[{[((<[[[<([<>[]](<><>))({{}()}[()()])>]>]><[{{{((<>[])<()()>)<[()[]]>}<([{}<>]({}())){{()[]}{[]<
{{{[{([<(<<[([()<>]<()()>)(<<>()>(()[]))]<<({}<>)[{}]><<[]<>>{{}[]}>>>>)>(([(<[{()<>}{{}()}]([(
({{{[({[{[({([{}<>]([]<>)){<<>[]>}})<[<[()]<[]>>]<<{{}[])[{}{}]>>>]}(<({{[<>{}]}([(){}]{{}<>})
{[(({{{([({({(()())[<><>]}{<[][]>(()[])}){[{<>()}[()[]]]<{<>[]}[[]{}]>}}[(<{[])[<>[]]>({{}[]}[()<>]))]){{[{<{")

(setf the-input-list (split-sequence:split-sequence #\newline the-input-string))

;;;part-1
(setf char-map (make-hash-table))

(setf   (gethash #\( char-map) 3
        (gethash #\) char-map) 3
        (gethash #\[ char-map) 57
        (gethash #\] char-map) 57
        (gethash #\{ char-map) 1197
        (gethash #\} char-map) 1197
        (gethash #\< char-map) 25137
        (gethash #\> char-map) 25137)

(setf   char-left-list  (list #\( #\[ #\{ #\<)
        char-right-list (list #\) #\] #\} #\>) )

(defun a (b)
    (let (char-stack)
        (block k
            (loop for i from 0 below (length b) do
                (let ((the-char (char b i)))
                    (if (member the-char char-left-list)
                        (push the-char char-stack)
                        (let (  (the-char-value (gethash the-char char-map))
                                (stack-char-value (gethash (pop char-stack) char-map)))
                            (if (= the-char-value stack-char-value)
                                nil
                                (return-from k the-char-value)))))))))

(defun b ()
    (print (apply #'+ (remove-if #'null (mapcar #'a the-input-list)))))

(time (b))  ;;369105
;;

;;;part-2
(setf number-map (make-hash-table))

(setf   (gethash #\( number-map) 1
        (gethash #\[ number-map) 2
        (gethash #\{ number-map) 3
        (gethash #\< number-map) 4)

(defun c (d e)
    (if d
        (c (cdr d) (+ (* e 5) (gethash (car d) number-map)))
        e))

(defun d ()
    (let ((e (sort 
                (mapcar 
                    (lambda (a) (c a 0))
                    (remove-if #'null 
                        (mapcar 
                            (lambda (a) 
                                (let (char-stack)
                                    (if (null (block b
                                            (loop for i from 0 below (length a) do
                                                    (let ((the-char (char a i)))
                                                        (if (member the-char char-left-list)
                                                            (push the-char char-stack)
                                                            (let (  (the-char-value (gethash the-char char-map))
                                                                    (stack-char-value (gethash (pop char-stack) char-map)))
                                                                (if (= the-char-value stack-char-value)
                                                                    nil
                                                                    (return-from b the-char-value))))))))
                                        char-stack 
                                        nil)))
                            the-input-list)))
                #'>)))
        (print (nth (floor (length e) 2) e))))

(time (d)) ;;3999363569
;;
