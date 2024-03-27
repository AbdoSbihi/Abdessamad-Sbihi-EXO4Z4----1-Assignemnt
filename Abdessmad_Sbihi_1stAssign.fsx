// Abdessamad Sbihi EXO4Z4 



///Numbers, sequences/lists, mapping, filtering, folding


// ex 1
let squareList (inputList: int list) =
    [for x in inputList -> x * x]

let inputList = [1 .. 10]
let result = squareList inputList
printfn "%A" result 





// ex 2
let filterOddNumbers (numbers: int list) : int list =
    List.filter (fun x -> x % 2 = 0) numbers
    


let numbers = [3 .. 15]
let evenNumbers = filterOddNumbers numbers
printfn "Even numbers: %A" evenNumbers





// ex 3
let sumOfPositiveNumbers (numbers2: int list) : int =
    List.sum (List.filter (fun x -> x > 0) numbers2)



let numbers3 = [-3; 5; -1; 7; -2; 10 ; 0 ; 12 ; -13]
let sumOfPositives = sumOfPositiveNumbers numbers3
printfn "Sum of positive numbers: %d" sumOfPositives





// ex 4
let capitalizeNames (names: string list) : string list =
    List.map (fun (name: string) -> name.ToUpper()) names

let names = [ "abdessamad"; "sbihi" ; "abdo" ; "bob" ; "emma" ; "ali" ]
let capitalizedNames = capitalizeNames names
printfn "Capitalized names: %A" capitalizedNames






// ex 5
let stringsWithLengthGreaterThanN (strings: string list) (n: int) : string list =
    List.filter (fun str -> String.length str > n) strings


let strings = ["school"; "street"; "wall"; "Parlement"; "lesson" ; "good" ; "midnight" ; "Dunaujvaros"]
let n = 6
let filteredStrings = stringsWithLengthGreaterThanN strings n
printfn "Strings with length greater than %d: %A" n filteredStrings






// ex 6
let countDivisibleBy divisor (numbers: int list) : int =
    List.filter (fun num -> num % divisor = 0) numbers |> List.length


let numbers4 = [1 .. 20]
let divisor = 4
let count = countDivisibleBy divisor numbers4
printfn "Count of numbers divisible by %d: %d" divisor count






// ex 7
let findIndicesOfElement element (list: 'a list) : int list =
    list
    |> List.mapi (fun index x -> if x = element then Some index else None)
    |> List.filter Option.isSome
    |> List.map Option.get


let numbers5 = [4; 2; 3; 6; 4; 2; 3 ; 10 ; 2 ; 4 ; 2]
let element = 4
let indices = findIndicesOfElement element numbers5
printfn "Indices of element %A: %A" element indices






// ex 8
let concatenateLongerStrings n2 (strings2: string list) : string =
    strings2
    |> List.filter (fun str -> String.length str > n2)
    |> String.concat ""


let strings2 = ["night"; "day"; "summer"; "winter"; "spring" ;"fall" ; "hours" ; "hungary"]
let n2 = 5
let concatenatedString = concatenateLongerStrings n2 strings2
printfn "Concatenated string of strings longer than %d: %s" n2 concatenatedString






// ex 9
let findMaxTuple (tuples: (int * int) list) : (int * int) option =
    match tuples with
    | [] -> None // Return None if the list is empty
    | _ ->
        // Find the tuple with the maximum value
        let maxTuple = List.maxBy snd tuples
        Some maxTuple


let tuples = [(1, 10); (2, 5); (3, 20); (4, 15) ; (5,30) ; (6,25)]
match findMaxTuple tuples with
| Some (id, value) -> printfn "Tuple with maximum value: (%i, %i)" id value
| None -> printfn "List is empty"






// ex 10 
let countOccurrences (list: 'a list) : ('a * int) list =
    list
    |> List.groupBy id
    |> List.map (fun (element, occurrences) -> (element, List.length occurrences))


let elements = [1; 2; 2; 2; 3; 3; 3; 3; 4; 4; 4 ; 4 ; 4;]
let occurrences = countOccurrences elements
printfn "Occurrences: %A" occurrences








/// Discriminated unions


// ex 11

type TrafficLight =
    | Red
    | Yellow
    | Green

let nextState (currentState: TrafficLight) : TrafficLight =
    match currentState with
    | Red -> Green
    | Green -> Yellow
    | Yellow -> Red


    


//ex 12

type ArithmeticOperation =
    | Add
    | Subtract
    | Multiply
    | Divide


let performOperation (operation: ArithmeticOperation) (x: float) (y: float) : float option =
    match operation with
    | Add -> Some (x + y)
    | Subtract -> Some (x - y)
    | Multiply -> Some (x * y)
    | Divide -> if y = 0.0 then None else Some (x / y)



let result1 = performOperation Add 6.5 4.0
let result2 = performOperation Subtract 12.0 10.1
let result3 = performOperation Multiply 16.0 3.0
let result4 = performOperation Divide 8.0 4.0


printfn "Addition result: %A" result1
printfn "Subtraction result: %A" result2
printfn "Multiplication result: %A" result3
printfn "Division result: %A" result4





// ex 13

type Shape =
    | Circle of radius: float
    | Rectangle of width: float * height: float
    | Square of side: float

let calculateArea (shape: Shape) : float =
    match shape with
    | Circle radius -> System.Math.PI * radius * radius
    | Rectangle (width, height) -> width * height
    | Square side -> side * side


let circle = Circle(2.2)
let rectangle = Rectangle(6.0, 8.0)
let square = Square(4.0)

printfn "Area of the circle: %.2f" (calculateArea circle)
printfn "Area of the rectangle: %.2f" (calculateArea rectangle)
printfn "Area of the square: %.2f" (calculateArea square)






// ex 14
type TemperatureScale =
    | Celsius
    | Fahrenheit

let convertTemperature (temperature: float) (fromScale: TemperatureScale) (toScale: TemperatureScale) : float =
    match fromScale, toScale with
    | Celsius, Fahrenheit -> (temperature * 9.0 / 5.0) + 32.0
    | Fahrenheit, Celsius -> (temperature - 32.0) * 5.0 / 9.0
    | _ -> temperature  // If the scales are the same, no conversion is needed


let celsiusTemperature = 22.0
let fahrenheitTemperature = 73.0

let convertedToFahrenheit = convertTemperature celsiusTemperature Celsius Fahrenheit
let convertedToCelsius = convertTemperature fahrenheitTemperature Fahrenheit Celsius

printfn "%.1f degrees Celsius is %.1f degrees Fahrenheit" celsiusTemperature convertedToFahrenheit
printfn "%.1f degrees Fahrenheit is %.1f degrees Celsius" fahrenheitTemperature convertedToCelsius





// ex 15
type JsonValue =
    | JsonObject of (string * JsonValue) list
    | JsonArray of JsonValue list
    | JsonString of string
    | JsonNumber of float
    | JsonBoolean of bool

let rec prettyPrint (json: JsonValue) : string =
    match json with
    | JsonObject items ->
        let itemsAsString = List.map (fun (key, value) -> sprintf "\"%s\": %s" key (prettyPrint value)) items
        sprintf "{ %s }" (String.concat ", " itemsAsString)
    | JsonArray items ->
        let itemsAsString = List.map prettyPrint items
        sprintf "[ %s ]" (String.concat ", " itemsAsString)
    | JsonString str -> sprintf "\"%s\"" str
    | JsonNumber num -> sprintf "%f" num
    | JsonBoolean b -> if b then "true" else "false"

let json =
    JsonObject [
        ("name", JsonString "John");
        ("age", JsonNumber 30.0);
        ("isStudent", JsonBoolean false);
        ("grades", JsonArray [JsonNumber 85.0; JsonNumber 90.0; JsonNumber 75.0])
    ]

let jsonString = prettyPrint json
printfn "%s" jsonString







/// Functions, recursion



// ex 16

let rec fibonacci n3 =
    if n3 <= 1 then
        n3
    else
        fibonacci (n3 - 1) + fibonacci (n3 - 2)


let n3 = 8
let fib = fibonacci n3
printfn "The %dth Fibonacci number is %d" n3 fib





// ex 17
let rec binarySearch (arr: int[]) (target: int) (low: int) (high: int) : int option =
    if low > high then
        None
    else
        let mid = (low + high) / 2
        match arr.[mid] with
        | midValue when midValue = target -> Some mid
        | midValue when midValue > target -> binarySearch arr target low (mid - 1)
        | _ -> binarySearch arr target (mid + 1) high


let sortedArray = [|0; 2; 5; 6; 9; 11; 12; 16; 18; 23|]
let target = 18
match binarySearch sortedArray target 0 (Array.length sortedArray - 1) with
| Some index -> printfn "Element %d found at index %d" target index
| None -> printfn "Element %d not found in the array" target





//ex 18
let rec mergeSort (lst: int list) : int list =
    let rec merge (left: int list) (right: int list) : int list =
        match left, right with
        | [], _ -> right
        | _, [] -> left
        | x::xs, y::ys ->
            if x < y then
                x :: merge xs (y::ys)
            else
                y :: merge (x::xs) ys

    let rec split (lst: int list) : int list * int list =
        match lst with
        | [] -> [], []
        | [x] -> [x], []
        | x::y::rest ->
            let left, right = split rest
            x::left, y::right

    match lst with
    | [] -> []
    | [x] -> [x]
    | _ ->
        let left, right = split lst
        merge (mergeSort left) (mergeSort right)


let lst = [5; 3; 8; 2; 7; 1; 4; 6]
let sortedList = mergeSort lst
printfn "Sorted list: %A" sortedList






// ex 19
type BinaryTree =
    | Node of value: int * left: BinaryTree * right: BinaryTree
    | Empty

let rec depth (tree: BinaryTree) : int =
    match tree with
    | Empty -> 0
    | Node(_, left, right) -> 1 + max (depth left) (depth right)


let tree =
    Node(1,
        Node(2,
            Node(4, Empty, Empty),
            Node(5, Empty, Empty)),
        Node(3,
            Node(6, Empty, Empty),
            Empty))

let treeDepth = depth tree
printfn "Depth of the binary tree: %d" treeDepth





// ex 20 
let rec isPalindrome (s: string) : bool =
    match s.Length with
    | len when len <= 1 -> true  // Base case: an empty string or a string with one character is a palindrome
    | _ when s.[0] <> s.[s.Length - 1] -> false // If the first and last characters don't match, it's not a palindrome
    | _ -> isPalindrome (s.[1..s.Length - 2]) // Recursively check the substring excluding the first and last characters


printfn "%b" (isPalindrome "radar")  // true
printfn "%b" (isPalindrome "hello")  // false

































  

   

