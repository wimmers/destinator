/* State declaration */
type state = {
  labels: list(string),
  label: option(string),
  weissbier: bool,
  input: string,
};



/* Action declaration */
type action =
  | Add(string)
  | Choose
  | Weissbier
  | UltimateWeissbier
  | UpdateInput(string);

let component = ReasonReact.reducerComponent("Example");

let s_special = Js.String.fromCodePoint(0x00df);

let weissbier_lit = "Wei" ++ s_special ++ "bier";

let make = (_children) => {
  /* spread the other default fields of component here and override a few */
  ...component,

  initialState: () => {labels: [], label: None, weissbier: false, input: ""},

  /* State transitions */
  reducer: (action, state) =>
    switch (action) {
    | Add(s) => ReasonReact.Update({...state, labels: [s, ...state.labels]})
    | Choose => List.length(state.labels) > 0 ? {
      let choice = List.nth(state.labels, Util.random_int(List.length(state.labels)));
      ReasonReact.Update({...state, label: Some(choice)})
      } : ReasonReact.Update({...state, label: None})
    | UpdateInput(s) => ReasonReact.Update({...state, input: s})
    | Weissbier => ReasonReact.Update({...state, labels: [weissbier_lit, ...state.labels]})
    | UltimateWeissbier => ReasonReact.Update({...state, weissbier: !(state.weissbier)})
    },

  render: self => {
    let new_label = self.state.input;
    let message =
      "Add";
    self.state.weissbier?
    (<div>
    <h2> (Util.str(weissbier_lit)) </h2>
    <img src="./src/weissbier.png"/>
    <br/>
      <button onClick=(_event => self.send(UltimateWeissbier))>
      <h2>
        (Util.str("Backtonormal"))
       </h2>
      </button>
    </div>)
    :
    (<div>
    
    <h2> (Util.str("Destinator")) </h2>
    <input
    _type="text"
    rows=1
    cols=5
    onChange=(evt => self.send(UpdateInput(Util.valueFromEvent(evt))))
    value=(self.state.input)
  />

  <br/>
    
      <button onClick=(_event => self.send(Add(new_label)))>
        (Util.str(message))
      </button>
      <button onClick=(_event => self.send(Choose))>
        (Util.str("Choose"))
      </button>
      <button onClick=(_event => self.send(Weissbier))>
       (Util.str(weissbier_lit))
      </button>

      <br/>
      <button onClick=(_event => self.send(UltimateWeissbier))>
      
        (Util.str("ULTIMATE"))
        <br/>
       (Util.str(weissbier_lit))
       
      </button>
      
      <br/>
      
      (
      List.map(
            label =>
            <li>(Util.str(label))</li>,
            self.state.labels
          )
          |> Array.of_list
          |> ReasonReact.arrayToElement
      )
      <br/>
      (
        switch (self.state.label) {
        | None => ReasonReact.nullElement
        | Some(label) => <div> (Util.str("-----------------")) <br/> <br/> (Util.str("You've chosen")) </div>
        }
      )
      <br/>
      (
        switch (self.state.label) {
        | None => ReasonReact.nullElement
        | Some(label) => <div> <h2> (Util.str(label)) </h2> </div>
        }
      )
      
      
    
    </div>);
  },
};
