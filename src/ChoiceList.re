/* State declaration */
type state = {
  labels: list(string),
  label: option(string),
  input: string
};

/* Action declaration */
type action =
  | Add(string)
  | Choose
  | UpdateInput(string);

/* Component template declaration.
   Needs to be **after** state and action declarations! */
let component = ReasonReact.reducerComponent("Example");

/* greeting and children are props. `children` isn't used, therefore ignored.
   We ignore it by prepending it with an underscore */
let make = (_children) => {
  /* spread the other default fields of component here and override a few */
  ...component,

  initialState: () => {labels: [], label: None, input: ""},

  /* State transitions */
  reducer: (action, state) =>
    switch (action) {
    | Add(s) => ReasonReact.Update({...state, labels: [s, ...state.labels]})
    | Choose => List.length(state.labels) > 0 ? {
      let choice = List.nth(state.labels, Util.random_int(List.length(state.labels)));
      ReasonReact.Update({...state, label: Some(choice)})
      } : ReasonReact.Update({...state, label: None})
    | UpdateInput(s) => ReasonReact.Update({...state, input: s})
    },

  render: self => {
    let new_label = self.state.input;
    let message =
      "Add: " ++ new_label;
    <div>
    <input
    _type="text"
    rows=1
    cols=5
    onChange=(evt => self.send(UpdateInput(Util.valueFromEvent(evt))))
    value=(self.state.input)
  />
      <button onClick=(_event => self.send(Add(new_label)))>
        (Util.str(message))
      </button>
      (
      List.map(
            label =>
            <div> (Util.str(label)) </div>,
            self.state.labels
          )
          |> Array.of_list
          |> ReasonReact.arrayToElement
      )
      <button onClick=(_event => self.send(Choose))>
        (Util.str("Choose"))
      </button>
      (
        switch (self.state.label) {
        | None => ReasonReact.nullElement
        | Some(label) => <div> (Util.str(label)) </div>
        }
      )
    </div>;
  },
};
