
import { h, Component } from "preact";
import cls from "classnames";

import { Search } from "./Search";
import { Preview, Report } from "./Results";
import { ExtSearch } from "./ExtSearch";


export class App extends Component {
  constructor() {
    super();
    this.state = {
      preview: null,
      report: null
    };
  }


  // TODO: Поиск по VIN / handle "notFound"
  render() {
    const {preview, report} = this.state;
    const mainVerticalCls = cls({vcentered: !preview});

    return (
      <div class={"main columns is-centered " + mainVerticalCls}>
        <div class="column is-half">
          <Search
            onPreview={p => this.setState({preview: p, report: null})}
          />

          { preview && <Preview preview={preview} /> }

          { (preview && !report) &&
            <ExtSearch
              previewId={preview.previewId}
              onReport={r => this.setState({report: r})}
            />
          }

          { report && <Report report={report} /> }
        </div>
      </div>);
  }
}
