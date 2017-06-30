import React from 'react';
import styles from './Sidebar.scss';
import TabMenu from './TabMenu/TabMenu';

class Sidebar extends React.Component {
  constructor(props) {
    super(props);
    this.state = { view: null };

    this.selectView = this.selectView.bind(this);
  }

  selectView(selectedView) {
    this.setState((previous) => {
      let { view } = previous;
      view = (view === selectedView ? null : selectedView);
      return Object.assign({}, previous, { view });
    });
  }

  render() {
    return (
      <div className={styles.Sidebar}>
        { this.state.view }
        <TabMenu callback={this.selectView} selected={this.state.view} />
      </div>
    );
  }
}

export default Sidebar;
