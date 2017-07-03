import React, { Component } from 'react';
import PropTypes from 'prop-types';

import SystemMenu from '../../SystemMenu/SystemMenu';
import TabMenuItem from './TabMenuItem';
import Icon from '../../common/Icon/Icon';
import SmallLabel from '../../common/SmallLabel/SmallLabel';
import styles from './TabMenu.scss';

class TabMenu extends Component {
  constructor(props) {
    super(props);
    this.callback = props.callback;
    this.clickTab = this.clickTab.bind(this);
  }

  isActive(tab) {
    return this.props.selected === tab;
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
          <SmallLabel>Playlist</SmallLabel>
        </TabMenuItem>
        <TabMenuItem active={this.isActive('view')} onClick={this.clickTab('view')}>
          <Icon icon="layers" />
          <SmallLabel>View</SmallLabel>
        </TabMenuItem>
        <TabMenuItem active={this.isActive('settings')} onClick={this.clickTab('settings')}>
          <Icon icon="settings" />
          <SmallLabel>Settings</SmallLabel>
        </TabMenuItem>
      </div>
    );
  }
}

TabMenu.propTypes = {
  callback: PropTypes.func,
  selected: PropTypes.node,
};

TabMenu.defaultProps = {
  callback: (() => {}),
  selected: null,
};

export default TabMenu;
