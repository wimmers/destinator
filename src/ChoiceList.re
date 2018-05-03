/* State declaration */
type state = {
  labels: list(string),
  label: option(string),
  weissbier: bool,
  input: string,
};

let changeThisBoolPlease = asd => !asd;


/* Action declaration */
type action =
  | Add(string)
  | Choose
  | Weissbier
  | UltimateWeissbier
  | UpdateInput(string);

/* Component template declaration.
   Needs to be **after** state and action declarations! */
let component = ReasonReact.reducerComponent("Example");

/* greeting and children are props. `children` isn't used, therefore ignored.
   We ignore it by prepending it with an underscore */
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
    | Weissbier => ReasonReact.Update({...state, labels: ["Weissbier", ...state.labels]})
    | UltimateWeissbier => ReasonReact.Update({...state, weissbier: changeThisBoolPlease(state.weissbier)})
    },

  render: self => {
    let new_label = self.state.input;
    let message =
      "Add";
    let destinatorstring = "Destinator";
    self.state.weissbier?
    (<div>
    <h2> (Util.str("Weissbier")) </h2>
    <img src="/Users/expert239/Desktop/JSlearning/destinator-master/src/weissbier.png"/>
    <br/>
      <button onClick=(_event => self.send(UltimateWeissbier))>
      <h2>
        (Util.str("Backtonormal"))
       </h2>
      </button>
    </div>)
    :
    (<div>
    
    <h2> (Util.str(destinatorstring)) </h2>
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
       (Util.str("Weissbier"))
      </button>

      <br/>
      <button onClick=(_event => self.send(UltimateWeissbier))>
      
        (Util.str("ULTIMATE"))
        <br/>
       (Util.str("Weissbier"))
       
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
