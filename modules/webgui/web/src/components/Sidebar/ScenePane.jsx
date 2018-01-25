import React, { Component } from 'react';
import PropTypes from 'prop-types';
import Pane from './Pane';
import { connect } from 'react-redux';
import DataManager from '../../api/DataManager';
import FilterList from '../common/FilterList/FilterList';
import SceneGraphNode from './SceneGraphNode';
import LoadingBlocks from '../common/LoadingBlock/LoadingBlocks';
import { insertInSceneGraph } from '../../api/Actions'

const NODES_KEY = '__allNodes';

class ScenePane extends Component {
  constructor(props) {
    super(props);
  }

  render() {
    const { nodes } = this.props;
    return (
      <Pane title="View" closeCallback={this.props.closeCallback}>
        { (nodes.length == 0) && (
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
    const rootNodes = state.propertyTree.filter(element => element.name == sceneType)
    let nodes = [];
    rootNodes.forEach(function(node) {
      nodes = [...nodes, ...node.subowners]; 
    })
    return {
        nodes,
    }
};

ScenePane = connect(
  mapStateToProps,
  )(ScenePane)

export default ScenePane;
