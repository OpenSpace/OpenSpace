import React, { Component } from 'react';
import PropTypes from 'prop-types';
import PropertyOwner from './Properties/PropertyOwner';
import ToggleContent from '../common/ToggleContent/ToggleContent';
import Button from '../common/Input/Button/Button';
import Icon from '../common/Icon/Icon';
import DataManager from '../../api/DataManager';
import { OriginKey } from '../../api/keys';

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
    const { name, subowners } = this.props;

    return (
      <ToggleContent title={name}>
        <Button onClick={this.focusOnThis}>
          <Icon icon="gps_fixed" /> Focus
        </Button>
        { subowners.map(sub => (
          <PropertyOwner
            key={sub.name}
            {...sub}
          />
        )) }
      </ToggleContent>
    );
  }
}

SceneGraphNode.propTypes = {
  name: PropTypes.string.isRequired,
  // eslint-disable-next-line react/forbid-prop-types
  subowners: PropTypes.arrayOf(PropTypes.shape({
    name: PropTypes.string,
    description: PropTypes.string,
    properties: PropTypes.arrayOf(PropTypes.object),
  })),
};

SceneGraphNode.defaultProps = {
  properties: [],
  subowners: [],
};

export default SceneGraphNode;
