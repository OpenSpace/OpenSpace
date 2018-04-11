import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';

import FocusButton from './FocusButton';
import { OriginKey, SolarSystemKey, StoryKey } from '../../../api/keys';
import { changePropertyValue, startListening, stopListening } from '../../../api/Actions';
import { traverseTreeWithURI } from '../../../utils/propertyTreeHelpers';
import styles from './FocusMenu.scss';
//import OverViewButton from './OverViewButton';

class FocusMenu extends Component {
  constructor(props) {
    super(props);

    this.state = {
      origin: '',
      //solarSystem: '',
      listening: false,
    };
  }

  componentDidUpdate(nextProps, nextState) {
    // If a focus button is clicked change property value
    if (this.state.listening && nextState.origin !== this.state.origin) {
      this.props.ChangePropertyValue(this.props.originNode.Description, this.state.origin);
    }
    // If the overview button is clicked change property value
/*    if (this.state.listening && nextState.solarSystem !== this.state.solarSystem) {
      this.props.ChangePropertyValue(this.props.overview.Description, this.state.solarSystem);
    } */
    // If changes are made in another gui update state
    if (this.state.listening && nextState.origin !== this.props.originNode.Value) {
      this.setState({ origin: this.props.originNode.Value });
    }
    // If changes are made to the solar system prop in another gui update state
   /* if (this.state.listening && nextState.solarSystem !== this.props.overview.Value) {
      this.setState({ solarSystem: this.props.overview.Value });
    } */
    // Start listening on the origin property and the viewSolarSystem property
    if (!this.state.listening && this.props.nodes.length > 0) {
      this.props.StartListening(OriginKey);
      //this.props.StartListening(SolarSystemKey);
      this.setState({ 
        origin: this.props.originNode.Value, 
        //solarSystem: this.props.overview.Value,
        listening: true });
    }
  }

  componentWillUnmount() {
    this.props.StopListening(OriginKey);
    //this.props.StopListening(SolarSystemKey);
    this.setState({ listening: false });
  }

  createFocusButtons() {
    const { nodes } = this.props;
    const focusPicker = nodes
      .map(node =>
        (<FocusButton
          key={node.identifier}
          identifier={node.identifier}
          active={this.state.origin}
          onChangeOrigin={origin => this.setState({ origin })}
        />));
    return (focusPicker);
  }

  render() {
/*    {this.props.nodes.length > 0 &&
    <OverViewButton
      identifier={this.props.overview.Description.Identifier}
      value={(this.props.overview.Value === 'true' || this.props.overview.Value === '1')}
      onChangeView={newVal => this.setState({ solarSystem: newVal })}
    />}*/
    return (
      <div className={styles.FocusMenu}>
        {this.props.nodes.length > 0 && this.createFocusButtons()}
      </div>
    );
  }
}

const mapStateToProps = (state) => {
  const sceneType = 'Scene';
  let originNode = [];
  let nodes = [];
  //let overview = {};

  if (Object.keys(state.propertyTree).length !== 0) {
    const storyIdentifierNode = traverseTreeWithURI(state.propertyTree, StoryKey);
    const rootNodes = state.propertyTree.subowners.filter(element => element.identifier === sceneType);
    rootNodes.forEach((node) => {
      nodes = [...nodes, ...node.subowners];
    });
    nodes = nodes.filter(node => node.tag.some(tag => tag.includes(storyIdentifierNode.Value)))
      .map(node => Object.assign(node, { key: node.identifier }));
    originNode = traverseTreeWithURI(state.propertyTree, OriginKey);

    // Get the overview node for the overview of the solar system
    //overview = traverseTreeWithURI(state.propertyTree, SolarSystemKey);
  }
  return {
    nodes,
    originNode,
    //overview,
  };
};

const mapDispatchToProps = dispatch => ({
  ChangePropertyValue: (description, value) => {
    dispatch(changePropertyValue(description, value));
  },
  StartListening: (URI) => {
    dispatch(startListening(URI));
  },
  StopListening: (URI) => {
    dispatch(stopListening(URI));
  },
});

FocusMenu = connect(
  mapStateToProps,
  mapDispatchToProps,
)(FocusMenu);

FocusMenu.propTypes = {
  nodes: PropTypes.arrayOf(PropTypes.shape({
    identifier: PropTypes.string,
    Description: PropTypes.string,
    properties: PropTypes.arrayOf(PropTypes.object),
    subowners: PropTypes.arrayOf(PropTypes.object),
  })),
  originNode: PropTypes.arrayOf(PropTypes.shape({
    id: PropTypes.string,
    Description: PropTypes.string,
    Value: PropTypes.string,
    listeners: PropTypes.number,
  })),
  overview: PropTypes.objectOf(PropTypes.shape({
    Identifier: PropTypes.string,
    Description: PropTypes.string,
    Value: PropTypes.string,
  })),
  ChangePropertyValue: PropTypes.func,
  StartListening: PropTypes.func,
  StopListening: PropTypes.func,
};

FocusMenu.defaultProps = {
  nodes: [],
  properties: [],
  subowners: [],
  originNode: [],
  overview: {},
  Description: '',
  Value: '',
  ChangePropertyValue: null,
  StopListening: null,
  StartListening: null,
};

export default FocusMenu;
