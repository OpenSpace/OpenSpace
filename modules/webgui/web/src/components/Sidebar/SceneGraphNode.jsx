import React, { Component } from 'react';
import PropTypes from 'prop-types';
import PropertyCollection from './Properties/PropertyCollection';
import ToggleContent from '../common/ToggleContent/ToggleContent';
import Button from '../common/Input/Button';
import Icon from '../common/Icon/Icon';
import DataManager from '../../api/DataManager';
import { OriginKey } from '../../api/keys';
import styles from './SceneGraphNode.scss';

class SceneGraphNode extends Component {
  constructor(props) {
    super(props);

    this.focusOnThis = this.focusOnThis.bind(this);
  }

  focusOnThis() {
    const { name } = this.props;
    DataManager.setValue(OriginKey, name);
  }

  render() {
    const { name, renderable } = this.props;

    return (
      <ToggleContent title={name}>
        <Button onClick={this.focusOnThis}>
          <Icon icon="gps_fixed" /> Focus
        </Button>
        { renderable && (<PropertyCollection name="Renderable" properties={renderable.properties} />) }
      </ToggleContent>
    );
  }
}

SceneGraphNode.propTypes = {
  name: PropTypes.string.isRequired,
  // eslint-disable-next-line react/forbid-prop-types
  renderable: PropTypes.object,
};

SceneGraphNode.defaultProps = {
  properties: [],
  renderable: {},
};

export default SceneGraphNode;
