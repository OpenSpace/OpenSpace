import React, { Component } from 'react';

import SmallLabel from '../../common/SmallLabel/SmallLabel';
import Icon from '../../common/Icon/Icon';
import LoadingString from '../../common/LoadingString/LoadingString';
import Picker from '../Picker';
import Popover from '../../common/Popover/Popover';
import FilterList from '../../common/FilterList/FilterList';
import DataManager from '../../../api/DataManager';
import FocusEntry from './FocusEntry';

import styles from './OriginPicker.scss';

// key to subscribe to the current focus/origin
const ORIGIN_KEY = 'NavigationHandler.Origin';
// key to get all scene graph nodes
const SCENEGRAPH_KEY = '__allNodes';
// tag that each focusable node must have
const REQUIRED_TAGS = ['planet_solarSystem'];

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

  /**
   * Get all the scene graph nodes that has one of REQUIRED_TAGS in them
   * @returns {Array}
   */
  get sceneGraphNodes() {
    return this.state.sceneGraphNodes
      .filter(node => node.tags.some(tag => REQUIRED_TAGS.includes(tag)))
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
            <FilterList
              data={this.sceneGraphNodes}
              className={styles.list}
              searchText="Search the universe..."
              viewComponent={FocusEntry}
              active={this.origin}
            />
          </Popover>
        )}
      </div>
    );
  }
}

export default OriginPicker;
