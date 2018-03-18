import React, { Component } from 'react';
import { connect } from 'react-redux';
import PropTypes from 'prop-types';
import Pane from './Pane';
import FilterList from '../common/FilterList/FilterList';
import SceneGraphNode from './SceneGraphNode';
import LoadingBlocks from '../common/LoadingBlock/LoadingBlocks';

class ScenePane extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    const { nodes } = this.props;
    return (
      <Pane title="View" closeCallback={this.props.closeCallback}>
        { (nodes.length === 0) && (
          <LoadingBlocks className={Pane.styles.loading} />
        )}

        { nodes.length > 0 && (
          <FilterList data={nodes} viewComponent={SceneGraphNode} />
        )}
      </Pane>
    );
  }
}

ScenePane.propTypes = {
  closeCallback: PropTypes.func,
};

ScenePane.defaultProps = {
  closeCallback: null,
};


const mapStateToProps = (state) => {
  const sceneType = 'Scene';
  const rootNodes = state.propertyTree.subowners.filter(element => element.name === sceneType);
  let nodes = [];
  rootNodes.forEach((node) => {
    nodes = [...nodes, ...node.subowners];
  });
  return {
    nodes,
  };
};

ScenePane = connect(
  mapStateToProps,
)(ScenePane);

export default ScenePane;
