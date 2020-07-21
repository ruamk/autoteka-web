
import { h, Component } from "preact";
import cls from "classnames";

import { Search } from "./Search";
import { Preview } from "./Preview";
import { Package } from "./Package";


export class App extends Component {
  constructor() {
    super();
    this.state = {
      avail: null,
      preview: null,
      report: null
    };

    this.setPreview = this.setPreview.bind(this);
    this.getReport = this.getReport.bind(this);
  }


  setPreview(p) {
    this.setState({
      preview: p,
      report: null,
      package: null
    });
  }

  getReport(e) {
  }


  // TODO: Поиск по VIN / handle "notFound"
  render() {
    const {avail, preview, report} = this.state;
    const mainVerticalCls = cls({vcentered: !preview});

    return (
      <div class={"main columns is-centered " + mainVerticalCls}>
        <div class="column is-half">
          <Search onPreview={this.setPreview} />

          { preview && <Preview preview={preview} /> }

          { (preview && !report) &&
            <div class="field is-grouped">
              <div class="control">
                <Package onAvail={a => this.setState({avail: a})} />
              </div>
              <div class="control">
                <a
                  class={cls({disabled: !(avail > 0)})}
                  onClick={avail > 0 && this.getReport}
                >
                  Расширенный поиск
                </a>
              </div>
            </div>
          }
        </div>
      </div>);
  }
}
