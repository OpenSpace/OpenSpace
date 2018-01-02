import React from 'react';

import TabMenu from './TabMenu/TabMenu';
import SystemMenu from '../SystemMenu/SystemMenu';
import TabMenuItem from './TabMenu/TabMenuItem';
import Icon from '../common/Icon/Icon';
import SmallLabel from '../common/SmallLabel/SmallLabel';
import ViewPane from './ViewPane';
import SettingsPane from './SettingsPane';
import { AllPropertiesKey, AllScreenSpaceRenderablesKey } from '../../api/keys';
import { insertInSceneGraph } from '../../api/Actions'
import { connect } from 'react-redux';
import DataManager from '../../api/DataManager';
const NODES_KEY = '__allNodes';

import styles from './Sidebar.scss';

const views = {
  settings: SettingsPane,
  view: ViewPane,
};

class Sidebar extends React.Component {
  constructor(props) {
    super(props);
    this.state = { view: null };

    this.selectView = this.selectView.bind(this);
    this.isActive = this.isActive.bind(this);
  }

  componentDidMount() {
    DataManager.getValue(AllPropertiesKey, this.props.InsertSettings);
    DataManager.getValue(AllScreenSpaceRenderablesKey, this.props.InsertSettings);
    DataManager.getValue(NODES_KEY, this.props.InsertSceneGraph);
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
            <Icon className={styles.icon} icon="playlist_play" />
            <SmallLabel>Playlist</SmallLabel>
          </TabMenuItem>
          <TabMenuItem active={this.isActive('view')} onClick={this.selectView('view')}>
            <Icon className={styles.icon} icon="layers" />
            <SmallLabel>View</SmallLabel>
          </TabMenuItem>
          <TabMenuItem active={this.isActive('settings')} onClick={this.selectView('settings')}>
            <Icon className={styles.icon} icon="settings" />
            <SmallLabel>Settings</SmallLabel>
          </TabMenuItem>
        </TabMenu>
      </section>
    );
  }
}

const mapDispatchToProps = (dispatch) => {
  return {
    InsertSceneGraph: (data) => {
      dispatch(insertInSceneGraph(data));
    },
    InsertSettings: (data) => {
      dispatch(insertInSceneGraph(data.value));
    },
  }
}

Sidebar = connect(
  null,
  mapDispatchToProps,
  )(Sidebar)
export default Sidebar;
