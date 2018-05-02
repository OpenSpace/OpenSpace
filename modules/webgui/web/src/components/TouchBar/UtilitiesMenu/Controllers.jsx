import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';

import { OriginKey, SetGoToGeoScript, ValuePlaceholder } from '../../../api/keys';
import { traverseTreeWithURI } from '../../../utils/propertyTreeHelpers';
import { changePropertyValue } from '../../../api/Actions';
import DateController from './DateController';
import TimeController from './TimeController';
import SightsController from './SightsController';
import DataManager from '../../../api/DataManager';


// TODO Remove and replace with input from API
const dateList = [
  { date: '2017-09-16T08:00:00.00', info: 'Hurricane Irma', planet: 'Earth', location: { lat: '24.555059', long: ' -81.779984', att: '10000000' } },
  { date: '2017-09-08T08:00:00.00', info: 'Hurricane Jose', planet: 'Earth', location: { lat: '18.073099', long: '-63.082199', att: '10000000' } },
  { date: '2017-08-21T12:00:00.00', info: 'Solar Eclipse', planet: 'Earth', location: { lat: '29.951065', long: '-90.071533', att: '10000000' } },
];

const sightsList = [
  { place: 'Norrköping', planet: 'Earth', location: { lat: '58.5877', long: '16.1924', att: '1000000' } },
  { place: 'Salt Lake City', planet: 'Earth', location: { lat: '40.758701', long: '-111.876183', att: '1000000' } },
  { place: 'New York', planet: 'Earth', location: { lat: '40.730610', long: '-73.935242', att: '1000000' } },
  { place: 'Huygens Crater', planet: 'Mars', location: { lat: '-13.96', long: '55.58', att: '3000000' } }];

class Controllers extends Component {
  constructor(props) {
    super(props);

    this.onChangeSight = this.onChangeSight.bind(this);
  }

  onChangeSight(selected) {
    // Check if the sight is on the current focus, otherwise change focus node
    if (this.props.originNode !== selected.planet) {
      this.props.ChangePropertyValue(this.props.originNode.Description, selected.planet);
    }
    const script = SetGoToGeoScript.replace(ValuePlaceholder, `${selected.location.lat}, ${selected.location.long}, ${selected.location.att}`);
    DataManager.runScript(script);
  }

  render() {
    return (
      <div style={{ display: 'flex' }}>
        <TimeController />
        <DateController
          dateList={dateList}
          onChangeSight={this.onChangeSight}
        />
        <SightsController
          sightsList={sightsList}
          onChangeSight={this.onChangeSight}
        />
      </div>
    );
  }
}

const mapStateToProps = (state) => {
  let originNode = [];
  let nodes = [];
  const sceneType = 'Scene';

  if (Object.keys(state.propertyTree).length !== 0) {
    const rootNodes = state.propertyTree.subowners
      .filter(element => element.identifier === sceneType);
    rootNodes.forEach((node) => {
      nodes = [...nodes, ...node.subowners];
    });
    originNode = traverseTreeWithURI(state.propertyTree, OriginKey);
  }
  return {
    originNode,
  };
};

const mapDispatchToProps = dispatch => ({
  ChangePropertyValue: (description, value) => {
    dispatch(changePropertyValue(description, value));
  },
});

Controllers = connect(
  mapStateToProps,
  mapDispatchToProps,
)(Controllers);

Controllers.propTypes = {
  originNode: PropTypes.arrayOf(PropTypes.shape({
    id: PropTypes.string,
    Description: PropTypes.string,
    Value: PropTypes.string,
    listeners: PropTypes.number,
  })),
  ChangePropertyValue: PropTypes.func,
};

Controllers.defaultProps = {
  originNode: [],
  ChangePropertyValue: () => {},
};

export default Controllers;
