import React, { Component } from 'react';

import SmallLabel from '../common/SmallLabel/SmallLabel';
import Icon from '../common/Icon/Icon';
import LoadingString from '../common/LoadingString/LoadingString';
import Picker from './Picker';
import Popover from '../common/Popover/Popover';
import FilterList from '../common/FilterList/FilterList';
import DataManager from '../../api/DataManager';

import styles from './OriginPicker.scss';

// key to subscribe to the current focus/origin
const ORIGIN_KEY = 'NavigationHandler.Origin';
// key to get all scene graph nodes
const SCENEGRAPH_KEY = '__allNodes';
// tag that each focusable node must have
const REQUIRED_TAG = "planet_solarSystem";

class OriginPicker extends Component {
  constructor(props) {
    super(props);

    this.state = {
      origin: 'Mercury',
      hasOrigin: false,
      sceneGraphNodes: [],
      showPopover: false,
    };

    this.updateOrigin = this.updateOrigin.bind(this);
    this.togglePopover = this.togglePopover.bind(this);
  }

  componentDidMount() {
    DataManager.subscribe(ORIGIN_KEY, this.updateOrigin);
    DataManager.getValue(SCENEGRAPH_KEY, (sceneGraphNodes) => {
      this.setState({ sceneGraphNodes });
    });
  }

  componentWillUnmount() {
    DataManager.unsubscribe(ORIGIN_KEY, this.updateOrigin);
  }

  updateOrigin(data) {
    const { Value } = data;
    this.setState({ origin: Value, hasOrigin: Value !== '' });
  }

  togglePopover() {
    this.setState({ showPopover: !this.state.showPopover });
  }

  get icon() {
    // return this.state.origin ? null : (<Icon icon="language" className="extralarge" />);
    return (<Icon icon="language" className={`extralarge ${styles.Icon}`} />);
  }

  get origin() {
    return this.state.origin;
  }

  get sceneGraphNodes() {
    return this.state.sceneGraphNodes
      .filter(node => (node.tags && node.tags.includes(REQUIRED_TAG)))
      .map(node => Object.assign(node, { key: node.name }));
  }

  render() {
    const { hasOrigin, showPopover } = this.state;
    return (
      <div className={styles.OriginPicker}>
        <Picker onClick={this.togglePopover} role="button" tabIndex={0} className={styles.button}>
          { this.icon }
          <div className={Picker.Title}>
            <span className={Picker.Name}>
              <LoadingString loading={!hasOrigin}>
                { this.origin }
              </LoadingString>
            </span>
            <SmallLabel>Focus</SmallLabel>
          </div>
        </Picker>
        { showPopover && (
          <Popover closeCallback={this.togglePopover} title="Select focus" className={styles.popover}>
            <FilterList data={this.sceneGraphNodes} className={styles.list} />
          </Popover>
        )}
      </div>
    );
  }
}

export default OriginPicker;
