import { h, Component } from "preact";

export class Package extends Component {
  constructor(props) {
    super(props);
    this.state = { avail: null };
  }

  componentDidMount() {
    fetch("/autoteka/packet_info")
      .then(res => res.json())
      .then(res => this.setState(
        {avail: res.reportsCntRemain},
        () => this.props.onAvail(res.reportsCntRemain)
      ));
  }

  render() {
    const {avail} = this.state;
    const color =
      avail === null ? "info"
        : avail < 2 ? "danger"
        : avail < 10 ? "warning"
        : "primary";

    return (
      <div class="tags has-addons">
        <span class="tag">Доступно</span>
        <span class={`tag is-${color}`}>
          {avail !== null
            ? avail
            : (
              <span class="icon is-small is-right">
                <i class="fas fa-spinner fa-pulse"></i>
              </span>
            )
          }
        </span>
      </div>);
  }
}
