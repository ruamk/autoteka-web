import { h, render, Component } from "preact";
import "bulma/css/bulma.css";

class App extends Component {
  render() {
    return (
      <div class="columns">
        <div class="column is-three-fifths is-offset-one-fifth">
          Hello!
        </div>
      </div>);
  }
}

render(<App />, document.body);
