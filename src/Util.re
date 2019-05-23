let str = React.string;

let random_int = n => Js.Math.floor(Js.Math.random() *. float_of_int(n));

let valueFromEvent = evt : string => (
                                       evt
                                       |> ReactEventRe.Form.target
                                       |> ReactDOMRe.domElementToObj
                                     )##value;