import React, { Component } from 'react';
import { connect } from 'react-redux';

import SmallLabel from '../../common/SmallLabel/SmallLabel';
import Icon from '../../common/Icon/Icon';
import LoadingString from '../../common/LoadingString/LoadingString';
import Picker from '../Picker';
import Popover from '../../common/Popover/Popover';
import FilterList from '../../common/FilterList/FilterList';
import DataManager from '../../../api/DataManager';
import { OriginKey } from '../../../api/keys';
import FocusEntry from './FocusEntry';

import Earth from './images/earth.png';
import styles from './OriginPicker.scss';

const icons = {
  Earth,
};

// tag that each focusable node must have
const REQUIRED_TAG = 'GUI.Interesting';

class OriginPicker extends Component {
  constructor(props) {
    super(props);

    this.state = {
      origin: 'Earth',
      hasOrigin: false,
      sceneGraphNodes: [],
      showPopover: false,
    };

    this.updateOrigin = this.updateOrigin.bind(this);
    this.togglePopover = this.togglePopover.bind(this);
  }

  componentDidMount() {
    DataManager.subscribe(OriginKey, this.updateOrigin);
  }

  componentWillUnmount() {
    DataManager.unsubscribe(OriginKey, this.updateOrigin);
  }

  get icon() {
    const icon = icons[this.state.origin];
    if (icon) {
      return (
        <img src={icon} className={styles.iconImage} alt={this.state.origin} />
      );
    }
    return (<Icon icon="gps_fixed" className={styles.Icon} />);
  }

  get origin() {
    return this.state.origin;
  }

  updateOrigin(data) {
    const { Value } = data;
    this.setState({ origin: Value, hasOrigin: Value !== '' });
  }

  togglePopover() {
    this.setState({ showPopover: !this.state.showPopover });
  }

  render() {
    const { hasOrigin, showPopover } = this.state;
    const { nodes, favorites } = this.props;
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
              data={nodes}
              favorites={favorites}
              className={styles.list}
              searchText="Search the universe..."
              viewComponent={FocusEntry}
              active={this.origin}
              searchAutoFocus
            />
          </Popover>
        )}
      </div>
    );
  }
}

const mapStateToProps = (state) => {
  const sceneType = 'Scene';
  let nodes = [];
  let favorites = [];
  if (Object.keys(state.propertyTree).length !== 0) {
    const rootNodes = state.propertyTree.subowners.filter(
      element => element.identifier === sceneType
    );
    rootNodes.forEach((node) => {
      nodes = [...nodes, ...node.subowners];
    });
    favorites = nodes.filter(node => node.tag.some(tag => tag.includes(REQUIRED_TAG)))
      .map(node => Object.assign(node, { key: node.identifier }));
  }
  return {
    nodes,
    favorites
  };
};

OriginPicker = connect(
  mapStateToProps,
)(OriginPicker);

export default OriginPicker;
