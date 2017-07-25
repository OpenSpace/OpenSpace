import React from 'react';

import TabMenu from './TabMenu/TabMenu';
import SystemMenu from '../SystemMenu/SystemMenu';
import TabMenuItem from './TabMenu/TabMenuItem';
import Icon from '../common/Icon/Icon';
import SmallLabel from '../common/SmallLabel/SmallLabel';
import ViewPane from './ViewPane';

import styles from './Sidebar.scss';

const views = {
  view: ViewPane,
};

class Sidebar extends React.Component {
  constructor(props) {
    super(props);
    this.state = { view: null };

    this.selectView = this.selectView.bind(this);
    this.isActive = this.isActive.bind(this);
  }

  selectView(selectedView) {
    return () => {
      this.setState((previous) => {
        const view = (previous.view === selectedView ? null : selectedView);
        return Object.assign({}, previous, { view });
      });
    };
  }

  isActive(view) {
    return this.state.view === view;
  }

  render() {
    const { view } = this.state;
    const SelectedView = views[view];
    return (
      <section className={`${styles.Sidebar} ${view ? styles.active : ''}`}>
        { SelectedView && (<SelectedView closeCallback={this.selectView} />)}

        <TabMenu>
          <SystemMenu />

          <TabMenuItem active={this.isActive('play')} onClick={this.selectView('play')}>
            <Icon icon="playlist_play" />
            <SmallLabel>Playlist</SmallLabel>
          </TabMenuItem>
          <TabMenuItem active={this.isActive('view')} onClick={this.selectView('view')}>
            <Icon icon="layers" />
            <SmallLabel>View</SmallLabel>
          </TabMenuItem>
          <TabMenuItem active={this.isActive('settings')} onClick={this.selectView('settings')}>
            <Icon icon="settings" />
            <SmallLabel>Settings</SmallLabel>
          </TabMenuItem>
        </TabMenu>
      </section>
    );
  }
}

export default Sidebar;
