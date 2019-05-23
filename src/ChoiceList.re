open BsReactSelect;

type item = {
  .
  "value": string,
  "label": string,
};

type state = {
  labels: list(string),
  chosen: option(int),
  weissbier: bool,
  input: string,
  selected_item: option(item),
};

type action =
  | Add(string)
  | Choose
  | Select(option(item))
  | Weissbier
  | UltimateWeissbier
  | UpdateInput(string);

let options = [
  "ABC",
  "Test",
  "ETC",
];

let options_arr =
  List.mapi((i, s) => {"value": string_of_int(i), "label": s}, options) |> Array.of_list;

let component = ReasonReact.reducerComponent("Example");

let weissbier_lit = {j|Weißbier|j};

let blacklist = [
  "ABC",
  "Test"
];

let bad_choice_texts = [
  {j|Trink ein Weißbier und ziehe nicht über los!|j},
  {j|Wie wär's am Sonntag mit Weißbier trinken?|j},
  "How about... you try again?"
];

let filter_choices = choices => {
  let is_blacklisted = x => List.exists(y => y == x, blacklist);
  List.filter(x => !is_blacklisted(x), choices)
};

let choose = (choices, good) => {
  let rec doit = () => {
    let i = Util.random_int(List.length(choices));
    List.mem(List.nth(choices, i), good) ? i : doit()
  };
  doit()
};

let choose_from_list = xs => List.nth(xs, Util.random_int(List.length(xs)));

let do_bad_choice: state => state = state => {
  let text = choose_from_list(bad_choice_texts);
  let labels = [text, weissbier_lit, ...state.labels];
  let chosen = Some(0);
  {...state, labels, chosen}
};

let do_choice: state => state = state => (List.length(state.labels) > 0 ? {
  let good = filter_choices(state.labels);
  List.length(good) > 0 ?
    {...state, chosen: Some(choose(state.labels, good))}
  : do_bad_choice(state)  
  } : {...state, chosen: None});

let make = (_children) => {
  ...component,

  initialState: () => {labels: [], chosen: None, weissbier: false, input: "", selected_item: None},

  reducer: (action, state) =>
    switch (action) {
    | Add(s) =>
      String.length(String.trim(s)) > 0 ? ReasonReact.Update({...state, labels: [s, ...state.labels]}) : ReasonReact.NoUpdate
    | Choose => ReasonReact.Update(do_choice(state))
    | UpdateInput(s) => ReasonReact.Update({...state, input: s})
    | Weissbier => ReasonReact.Update({...state, labels: [weissbier_lit, ...state.labels]})
    | UltimateWeissbier => ReasonReact.Update({...state, weissbier: !(state.weissbier)})
    | Select(None) => ReasonReact.Update({...state, selected_item: None})
    | Select(Some(x)) => ReasonReact.Update({...state, selected_item: Some(x), input: x##label})
    },

  render: self => {
    let new_label = self.state.input;
    let message =
      "Add";
    <div className="container center-block">
    
      <h2 className="page-header"> (Util.str("Destinator")) </h2>
 
      <div className="container choices">
        <div className="row">
          <input
            type_="text"
            rows=1
            cols=5
            className="input"
            onChange=(evt => self.send(UpdateInput(Util.valueFromEvent(evt))))
            value=(self.state.input)
          />
        </div>
      
        <div className="row">
        <Select
          options=options_arr
          value=?
            self.state.selected_item->Belt.Option.map(item => Select.Option.Val(item))
          onChange=(selected => self.send(Select(selected)))
          arrowRenderer=(_ => <div> (ReasonReact.string("+")) </div>)
          filterOptions=(Func((~options, ~filter as _filter) => options))
          searchable=false
          clearable=false
          placeholder=(Str("Partei..."))
          className="input"
        />
        </div>

        <div className="btn-group row default-margin" role="group">
          <button 
            onClick=(_event => self.send(Add(new_label))) 
            className="btn btn-default"
          >
            (Util.str(message))
          </button>
          <button onClick=(_event => self.send(Choose)) className="btn btn-default">
            (Util.str("Choose"))
          </button>
          <button onClick=(_event => self.send(Weissbier)) className="btn btn-default">
            (Util.str(weissbier_lit))
          </button>
        </div>

        <ul className="list-group row default-margin">
        (
        List.mapi(
              (i, label) =>
              <li className=("list-group-item " ++ ((Some(i) == self.state.chosen)?"active":""))>(Util.str(label))</li>,
              self.state.labels
            )
            |> Array.of_list
            |> ReasonReact.array
        )
        </ul>
      </div>
    </div>;
  },
};
