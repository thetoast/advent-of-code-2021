module Day10Input where

testInput :: String
testInput =
  """[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"""

realInput :: String
realInput =
  """[[<[<{[<{{<[{(()[])[[]()]}[((){})<[]{}>]}<<{[]{}}<(){}>><([]<>)>>>[[[<<>>[<><>]][<<>>(<><>)]][{
((<[(<<(<{<((<()<>>{()()}){<<>><()[]>})([{<>{}}(()[])]{(<><>>})>(<((()())<[][]>)>([(<>{})({}())][<
<<<(<<[[((({<([]<>)[[]]>{<{}>{(){}}}}{[<[][]><<>[]>]({<><>}{()<>})})[<<({}())(()<>)>[([]{})<[]()>]>({[[]
<(<<<[((([{({((){})<<>{}>}[<(){}>[<>()]])[[<{}[]>({}{}]](<<>[]><[]<>>)]}](<[({[][]}<()<>>)<[
{<<<{[({{(<<(<<>[]><[]()>){<[][]>{<>{}}}>((({}())([]<>))[<{}{}>({}())])>(<<<()[]>><<<>{}>(()<
{<{<<[[(<<[{<<{}[]>>([<>()]{{}{}})}]<(<{<>()}{{}()}>{({}[])<<>()>})>>>){[{[{<[[]{}][<><>]>}{(<{}(
(((([(({((<[<(<>{})[{}{}]>](<[()<>]{<><>}>)><<[<[]{}>((){})]<[()[]]>>{[{<>()}((){})]({()[]}(()[]))}>))}(
<<[(({({(<{{((<>[])[[]<>])([<>]{()()})}<<{<>[]}><<{}()>>>}>)(<(<{<()[]><(){}>}{{(){}}[{}<>]}>{
(([[{<[<<{[<{<()[]>{<>[]}}{<{}{}>{[]<>}}>[{<{}{}>(<>[])}]]{<<({}[])>(<[]<>>[{}])>({{<>()}(<
(<{{(<<{[([<<{()[]}>{({}<>){<>[]}]>((<[]{}><{}{}>))]([{(()<>)(<>[])}]<<((){})<<>{}>>[(()[])
{{[[<<{<{{{([{()[]}[{}()]]{(<>{}){<><>}}){<[[][]][()[]]>}}<{{<[][]>[{}[]]>(<<>{}><{}{}>)}>}}<<{[{([](
{[<({{{[<{<{[([][])]{({}())<[][]>}}((<(){}>{()<>})<<<>{}>>)>}(((<{<>{}}<{}()>>)[((()()))[<<>{}>(
{{{[{{[(([{{[[{}<>]([]<>)]([(){}][[]()])}[{{[]<>}[{}()]}(<<><>>]]}][[([[[]<>]<<>{}>][{[]{}
<<{{{[{{[<({<<[]<>><{}<>>>}[[{()<>}{[]<>}]({(){}}{{}()})])[[(<<>[]>[[]])[{{}{}}[[]()]]]<((
({{[{{{{<[[({[{}{}]{[]<>}}<<()<>>(()<>)>)<{{(){}]<()()>}{{(){}}[<><>]}>]<<({{}{}}<(){}>)([{}
{([[[<<([[{{[<()<>>(()<>))({<>[]}(<>{}))}{(<{}()>{()[]})}}([[{{}[]}<[]>]<([]<>){<>}>])]<{[[([]{})]{(<><>)[[]<
([<<([<[[({((({}[]){[]{}}){({}[])({}<>)})>)[(<{{{}{}}[(){}]}(([][])<{}<>>)><[[<><>]{<>{}}]{{{}[]}{[][]}}>)<[[
{[<<{[<{[<{[[({}[])({}<>)]<<{}{}>{{}()}}]{[[<>{}]<[]{}>]}}(([(<><>){[][]}]([<>()]<()[]>))<[<()<>>(<><>)
(<[<[(({{<{<<[{}()]>(([]())<<>[]>)>}[{<[[]{}]([]<>)><{<><>}[()<>]>}{{{(){}}{[]{}}}}]>}}){{<{([<([]())({}())
<<{({{<[<[{([{{}<>}{<><>}]([(){}](()[]))){[{()[]}<<>()>]({()()}({}()))}}]<[<<(())(()[])>>{(<{}<>>{
{{{(<<[(<[([[[()<>]<()[]>]]{[[[]{}]{<>{}}]((<><>)({}{}))})]([(<{<>()}{[]{}}><<{}{}><<>[]>>)[<<()[]>((){
[{(<<{[<<([{[<[][]>][{()()}({}())]}]([{([]{})<{}<>>}(<()>[{}])]{<([]<>)<{}{}>>(({}<>)<<><>>)}))>([[((<<>{}
[[(<({{(({{(<[{}<>]([]{})>{[<>[]]([][])})[(<[]<>><{}()>)[<{}()>(())]]}<[<[[]{}]<{}{}>>{<()()>({}<>)}]<
([(<({(<{{(((<[]<>>{<>()})<{[]{}>([]{})>)[<[()<>]({}[])>[(<>{})(<>[])]]){<((<>{}){()[]})><{<[]<>><<><>>
({{<({(<<({[[{<><>}]<<<><>>({}[])>]([<[]<>>[{}()]]{{<>{}}({}[])}}}<{({[]<>}({}{}))[(<>())(<>())]}((([]{}){[][
(((({[(<([<[(<<>>[[][]])<{<><>}[{}<>]>]{({<>()}(<><>))(({}<>))}>])<([[<<()<>>{[][]}>]<[[[][
[<<(([({((<<{({}())}[(()()){[]<>}]>>){({<[{}{}]<<>()>>{<{}()>}})})})])[[<((<[{([<>()][{}[]])((()[])
{{{<{[[(<[{{<<{}()>[{}<>]>{({}{})<()>}}(<[(){}]>(<(){}>[[]]))}(<<[[]{}}({})><[<>[]]{<><>}>><[<[]<>>{<><>}](((
{<[[<[{{<((<[<<>()><[]{}>][<{}()><{}{}>]>)[[[[[][]]<()[]}][([]{})(()())]]])>[[((<{[]<>}<()>><[{}[]](<><>)>
<{([{[{[{([({<{}[]>([]<>)}[[[][]]<()()>])[<[{}()]>{<<><>>(()<>)}]]){({(<[]<>>([]<>))[(<>){
[{{<(({<{[{<{({})(<>[])}({<><>}{[]()})>(({[]<>}(<>{}))[{()()}{{}<>}])}{<{{<>()}<[]{})}(<()><(
{(<[{[[{{<(([{{}{}}<()[]>]<[<><>]>)([(<><>)])){<<{[]{}}[[][]]>{[[]<>]{<>{}}}>[<<[][]><<>()>><<{}<>>[<>()>>]}>
<[{{(<<{{{[[{<[]<>>(()<>)}<{()<>}{<>[]}>]<[<()()>]{<<>>([]())}>]{([<<>{}>({}<>)])[({{}}{[]<>})[
<<[[<<[(<{{{[{()[]}([]())][[[]]((){})]}}(([{[][]}[()()]]<<()>[{}[]]>))}>)<{({{<[[][]]{()()}><<{}{}
{[<(([([{({([({}()){<><>}][((){})([]())])[<<()<>>(<>())>{[[]()}(()[])}]}<{{<(){}>{()[]}}((<><>){{}<>})}((
{(({([[{{((([<()()>([][])]<<{}[]>[<>]>)(<{<><>}[{}{}]>[[<>{}]]))[{<<[][]>[()]><[{}{}]>}{<([]<>)([]<>)>}]
(({[[<<<{[{([(<>[])<()[]>])(<([])({}{})>)}[{(<<>{}>({}<>))}<<[<>[]>[(){}]>(<{}{}>)>]]{([[<<>()>
[{[[{([({[{(({()[]}[[]<>])<[[]<>]([])>)([{{}{}}[{}[]]]<{{}<>}>)][<(<<>[]>{<><>})<(()<>){[][]}>>[[{[]{}}([]())
([[{({([({<<(<[]<>>)[{()[]}<<>()>]><<{<>()}<<>[]>>[[<>{}]<<><>>]>)<<<<<>[]><<><>>>[[[]]]>[{
[{{([(<[[<{((([]{})[()()])[<[]{}>[<>()]])<[{()()}(<>{})]({[]}[<>{}])>}((<{[]()}>)({<{}{}>[
[[[{{[[[<<(<(({}())[<>{}])<{{}<>}(()())>>[[((){})][[[]{}]{()}]])>>{[(<([{}{}]({}<>))([{}()]<<>[]])>({<
{<{({(({{{[{<(<>{})>{<{}<>>(<><>)}}<(<[]()>[[][]])<[{}()]>>]<{{<()[]>{{}()}}{<{}[]>{{}{}}}}{{<()<>><<>
[{[<{[<<({(<{{<>()}({}<>)}<(<>[])[()()]>>{{{<>()}{()[]}}})<{<({}()){<>{}}>({<>[]}({}())]}>}(
[[{<([((({([{{()[]}<[]{}>}({<>[]}[<><>])]{{<[]<>>[<><>]}[{()()}[<>[]]]})}[{[<{()()}(<>))<({}){()<>}>]([
(<(<{[{<({[{{{[]()}({}{})}{<{}[]><<>{}>}}]})>}<{[[(<[<[]{}>([]())](<{}>[()()])>)][(([{[]<>
<<<<{<{<<[<({<[]>({}<>)})<[{[][]}[[]()]][[{}<>]<{}[]>])>([(([][])[[]()])<[{}[]]{[]{}}>](<[{}()]{{}()}
({[[([([<{[[({()[]}[<>[]])[<[][]><<>{}>>][(({}{})(<>[])){(<>[])}]]<{{{{}()}[<>[]]}<[[]<>]([]())>}>}{<{({
((<{<{{[((<<<(()<>)<[]{}>>[<[][]>[(){}]]>>)){([[[[<>]({}<>)]{{{}<>}([]{})}]<[<<>()>(()())][{[]{}}{[]<>}]>]
{<<{{[({[<{{(<{}[]><<>[]>)([[]()])}<[{{}{}}{<><>}]<({})<{}()>>>}<[([{}<>][<>{}])](<<{}[]><<
{[{[[<<[(({[(((){}){<>[]})]([[<>[]]][<<>{}>[<><>]])})({<[({}[])<{}[]>]>}<(<(()<>)[[]{}]][[()()][<><
((<<[({<{[[({<[]()>([]())}[<[]{}>(()<>)])]<<{[{}[]][<>()]}><<{<>[]}[()<>]>[<[]{}>{[]()}]>>][(({[<>
{{<[{<([({{([<<><>>{{}()}][<[][]>{[]()}])[<[[]]{[]<>}>]}(({<()[]>[{}()]}<[[]<>]{<>()}>))})])<{[{<[<[{}{}]
[(({({[[<{(<{{<>[]}[()<>]}((<>{})<<>[]>)>{[<(){}>{{}<>}]])[<{(<>[]){{}[]}}([()()][<>[]])>((({}())
{(([<{([[{([<<{}()><[][]>>[(<>()){()[]}]]<[<()()>[{}]]([{}<>][{}[]])>){<{[[]<>]<<>{}>}{([]{})}>[([
<{[{({([(<<{<[()<>]<(){}>>(<()>{[][]})})>{[([<{}{}>[<>()]])[{([]()){<>{}}}<<{}<>>>]]})])([<(([{<(){}>[{
<<<(([<[{[[<{({}()>{[]()}}>]]([<[[()][{}[]]]<[()()]{(){}}>><<({}[])(()<>)>{(()[])}>]{[(<[]()>)[{[]{}}
(({(([[<{(<{[(<>[]]]}<(({}{}))([[][]]([][]))>><[{(()())[()<>]}<{(){}}>]>)((<{{()[]}{<>()}}<([
[[[{<[{<[<[[{(<>{})<[]()>}{(()[])[<>()]}][<({}()>[<><>]>{<[]{}><[][]>}]]><<{[{(){}}{{}{}}][<<><
((([<<((<[{(([<>[]]([]{}))([<>]{<>()})}([[(){}][[]()]][(<><>){{}()}])}{[<[()<>]({}<>)>[<{}{}>]][<(<>())<[]
{<<{([(((([{<[()]{(){}}>{{{}[]}}}[{(()[]){()<>}}{<<>>[<>{}]}]]<[{(<>())(()[])}]>){[[<({}{})[()[]]>{[{}(
([{(({([([(<<{<>[]}<{}<>>>[[[]{}]<[]{}>]>((([]<>)<<>[]>)[[(){}]]))[<{{<>{}}}[(<>}{()()}]><{<<>{}><
{(<[<{({([{{([[][]]>({()()}[[][]])}[<(()())([]())><(<><>)([]{})>]}[(({{}{}}{{}<>})([<><>]<()[]>))]
((<(<<{{{{<{{(()<>)({}{})}{{<>[]}<()[]>}}<<<[][]>(()[])>[[()[]][[]()]]>>[[{{[]{}}[[][]]}][<[()[]]{[]<>}>
(((<(<<<{{<{<{{}{}}(()<>)>[((){})[()[]]]}{[[<>[]]{{}()}][{()[]}({}{})]}>}}>{[{<<{[{}()]}<[[]()]
({(([[<{[{<(([[]()](<><>))[<{}[]){{}()}])(<[()[]]<[]()>>[<{}<>>[()<>]])>[<[[()()]<<>[]>][([]())([]{})]>{{<<>(
[[(([((([<[[<{{}<>}({}[])>[([]<>)[()[]]]]<[(()())({}<>)](([]<>)<[]()>)>]>[{{{(()<>)}<{[]()}
(<(({[<<{((<[[[]()]{{}<>}]<<{}()>({}[])>>)<{{({}){()()}}<{[][]}>}>){[(<<[]<>>[{}{}]>{{(){}}{[
{<(<(<<{<(<{{[{}[]]([]{})}([{}[]](<>))}[([<>()]{[]{}})<({}<>){<><>}>]>[{<[[][]]>}[<[<><>](()())>(([
{(<(<{<(<<{{{<(){}>}({[][]}{{}{}})}}<(<<<>()>[(){}]>(<{}()>{{}}))>>[{{[{[]{}}(()[])]}[((<>())<<
([((([[{([{<<((){})>{{<>}{[]()}}>{[((){})<()[]>]<{[]{}}[(){}]>}}{((<{}()><{}>)){{<{}[]>[[]
{{{(<<{((([[{(<>{})({}())}[<[]()>({}{})]]<<[{}]>{[[]()]{()[]}}>]{({<()[]>((){})})<{<[]<>>{{}{}
(<<[[([{{(<<({{}}[{}{}])<([]{}){<>()}>>>(({<()()>({}[])}<<{}()>[(){}]>)))[{([{<>[]}([]())]([()()][<>{}])){
<(<(<<<([[[(<{{}[]}{{}[]}>[[<>()]{()[]}])]<{({{}()}{{}<>})<({}[])([]<>)>}{(({}[])(<>{}))((()<>)<[]()>)}>](<
<<{(([<([[[[<{<>()}({}[])>{((){})[{}()]}]}[<{<<>[]><[]<>>}<[{}<>]{{}()}>>{{(()())[()[]]}{<()<>
(({<<[{([{<(([{}[]][(){}])[<[]()>(()<>)]}>}<[(<{{}()}{{}<>}><[[]()][[]()]>)]<(<(<>())>({<>[]})){[{(){}}<{}{}>
({[[{[{{<(([[[{}{}]<(){}>]((()[])(()[]))]({([]<>)({}{})}{<<>{}>{<>}}))<{(<[]{}>(()()))[[()[]](<><>)
([{[{{<{{(<[((<>){[]()})(({}())[<><>])]>({[(<><>}{{}[]}]{(()[])[()]}}{([[]{}](<><>))<<{}[]>(<>())>}))
<[(([[[<{[[(({()[]}<<>[]>){{<><>}<(){}>})<([()]{<>[]}){{<>{}}}>]]<[{{([]())({}{}>}<{[][]}<<>{}>
({(<{<([[<{[<{[]()}[{}<>>>[[(){}][[]{}]]][{[()<>][<><>]}{<()()>}]}<({{(){}}<()()>}{[<><>](<>())})[(
([({<[<[{({<<[<>{})[<><>]>{{[]{}}}>[[{()<>}([]{})](({}<>)<<>>)]}<{<<<>>{()[]}>}<{<[]()>}>>)(<<<{
({<{[[({[([{[{[]{}}{[]<>}](((){})({}()))}]([<{<>{}}{()<>}>{({}{})(()())}])}]<{({{<()[]>{()}}
({[[{[((<[[[{<<>[]>(<>[])}]]{<({{}[]}){[{}{}]<<>()>}>[([<>()]{{}()})(<()()><<>[]>)]}]>(<([{<[]{}>(()())}<{[
[<<<{<({({{<{[<><>]{{}<>}}{<(){}>}>}})})[<<[{[{({}{})<<>>}[{[]}(()())]]{{([]())([]())}{({}[])[<>
<<<[[((<{{<(((<>[])(()))<<<>()><<>{}>>)<[{<>[]}{(){}}]{[{}]({}())}>>}}[[((({()}{[][]}){<{}[]>}
(<[<({[[{[<<[(<>[])]([[]<>]<[]{}>)>({(<>[])([]())}<[{}<>)<<>{}>>)>[{<[[][]]><[{}<>]<[]()>>}<<[
{[({<<<<(<([{{[][]}}]{<{<>()}<<>()>>[({}())<[]{}>]})>)>>><<<{{((([{}[]]<<><>>)[[[][]]({}[])])<{<{}{}>(
[[[({((({<([({[][]}{[]})<({}[]}[{}{}]>][([[]()]([]()))<<()<>>>])<[<<()[]><()[]>>]>>([<<{[]()}{[][]}><
[{{<<<({<({(<<<><>>>{(<>[])[<><>]})<(<<>{}>(()()))[{{}()}{(){}}]}}[(<{[]()}<[]<>>>({<>{}}[{}
([<<{<[{([<[<{<>{}}<[]{}>><([][]>>][{([][])[()()]}<<[]<>>>]>][{<({<>()}<[][]>)[[[]<>]{()<>}
({<<[<(<<<(<((()<>)<{}<>>)>)<<([<>{}][<>[]])<<()()>({}[])>>([(()())(()<>)]{{{}<>}<[]{}>})>>>>[
<{[{<[[<[(<{({<><>}{[]<>}){<[]{}><()>}}({({})[[]{}]})>{(<{[]<>}{<><>}>([{}[]]{<><>}))(<<{}()>[
<{{<[[({{{<({<[]()>[{}[]]}<{<><>}{()<>}>)[<<[]{}>{{}<>}>[<<>[]>((){})]]>}}(<[[(([][])[{}<>]){(()<>)[()[]]}]((
[{[{<[(({{({<{{}{}}<()()>>(<[]<>>{<>[]})}(<<()[]>{()<>}>))<<<[{}{}]<<>[]>>>[[{[]{}}]((()<>){<>
{[[<{({[<(<((<{}[]>){[{}[]]})[{[{}{}]({}())}]>(<{{()<>}(<><>)}[(<><>)<[]{}]]><{<()[]>([][])"""
