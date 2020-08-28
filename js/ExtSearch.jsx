import { h, Component } from "preact";
import cls from "classnames";

import { Package } from "./Package";
import { Message, ok, info, err } from "./Message";


export class ExtSearch extends Component {
  constructor(props) {
    super(props);
    this.state = {
      avail: null,
      status: "ready"

    };
    this.getReport = this.getReport.bind(this);
  }

  getReport() {
    const {previewId, onReport} = this.props;
    this.setState({status: "inProgress"})

    const startTime = new Date();
    const timeDelta = () => (new Date() - startTime) / 1000;
    fetch(`/autoteka/report/${previewId}`)
      .then(res => res.json())
      .then(res => this.setState(
        {status: "done"},
        () => onReport({reqTime: timeDelta(), ...res})))
      .catch(() => this.setState({status: "error"}));
  }


  render() {
    const {avail, status} = this.state;
    const canSearch = status === "ready" && avail > 0;

    return (
      <div class="field is-grouped">
        <div class="control">
          <Package onAvail={a => this.setState({avail: a})} />
        </div>
        <div class="control">
          <a
            class={cls({disabled: !canSearch})}
            onClick={canSearch && this.getReport}
          >
            Расширенный поиск
          </a>
        </div>
        { status === "inProgress" &&
          <Message {...info("Это может занять несколько минут...")} />
        }
        { status === "error" &&
          <Message {...info("Что-то пошло не так.")} />
        }
      </div>);
  }
}
