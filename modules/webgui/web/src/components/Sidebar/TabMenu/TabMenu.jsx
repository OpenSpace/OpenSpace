import React, { Component } from 'react';
import PropTypes from 'prop-types';

import SystemMenu from '../../SystemMenu/SystemMenu';
import TabMenuItem from './TabMenuItem';
import Icon from '../../common/Icon/Icon';
import styles from './TabMenu.scss';

class TabMenu extends Component {
  constructor(props) {
    super(props);
    this.state = { activeTab: null };
    this.callback = props.callback || (() => {});
    this.clickTab = this.clickTab.bind(this);
  }

  componentWillReceiveProps({ selected }) {
    this.setState(previous => Object.assign(previous, { activeTab: selected }));
  }

  isActive(tab) {
    return this.state.activeTab === tab;
  }

  clickTab(selectedTab) {
    return () => this.callback(selectedTab);
  }

  render() {
    return (
      <div className={styles.TabMenu}>
        <SystemMenu />

        <TabMenuItem active={this.isActive('play')} onClick={this.clickTab('play')}>
          <Icon icon="playlist_play" />
          <span>Playlist</span>
        </TabMenuItem>
        <TabMenuItem active={this.isActive('view')} onClick={this.clickTab('view')}>
          <Icon icon="layers" />
          <span>View</span>
        </TabMenuItem>
        <TabMenuItem active={this.isActive('settings')} onClick={this.clickTab('settings')}>
          <Icon icon="settings" />
          <span>Settings</span>
        </TabMenuItem>
      </div>
    );
  }
}

TabMenu.propTypes = {
  callback: PropTypes.func,
};

TabMenu.defaultProps = {
  callback: (() => {}),
};

export default TabMenu;
