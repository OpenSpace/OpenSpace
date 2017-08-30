import React, { Component } from 'react';

import SmallLabel from '../../common/SmallLabel/SmallLabel';
import Icon from '../../common/Icon/Icon';
import LoadingString from '../../common/LoadingString/LoadingString';
import Picker from '../Picker';
import Popover from '../../common/Popover/Popover';
import FilterList from '../../common/FilterList/FilterList';
import DataManager from '../../../api/DataManager';
import { OriginKey, SceneGraphKey } from '../../../api/keys';
import FocusEntry from './FocusEntry';

import Earth from './images/earth.png';
import styles from './OriginPicker.scss';

const icons = {
  Earth,
};

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
    DataManager.subscribe(OriginKey, this.updateOrigin);
    DataManager.getValue(SceneGraphKey, (sceneGraphNodes) => {
      this.setState({ sceneGraphNodes });
    });
  }

  componentWillUnmount() {
    DataManager.unsubscribe(OriginKey, this.updateOrigin);
  }

  updateOrigin(data) {
    const { Value } = data;
    this.setState({ origin: Value, hasOrigin: Value !== '' });
  }

  togglePopover() {
    this.setState({ showPopover: !this.state.showPopover });
  }

  get icon() {
    const icon = icons[this.state.origin];
    if (icon) {
      return (
        <img src={icon} className={styles.iconImage} alt={this.state.origin} />
      );
    }
    return (<Icon icon="language" className={styles.Icon} />);
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
      <div className={Picker.Wrapper}>
        <Picker onClick={this.togglePopover} className={(showPopover ? Picker.Active : '')}>
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
          <Popover closeCallback={this.togglePopover} title="Select focus" className={Picker.Popover}>
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
