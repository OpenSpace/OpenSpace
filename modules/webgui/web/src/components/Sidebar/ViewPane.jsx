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

class ViewPane extends Component {
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

ViewPane.propTypes = {
  closeCallback: PropTypes.func,
};

ViewPane.defaultProps = {
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

ViewPane = connect(
  mapStateToProps,
  )(ViewPane)

export default ViewPane;
