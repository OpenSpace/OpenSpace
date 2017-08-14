import React from 'react';
import PropTypes from 'prop-types';
import ToggleContent from '../../common/ToggleContent/ToggleContent';
import Property from './Property';

const PropertyCollection = ({ name, properties }) => (
  <ToggleContent title={name}>
    { properties.map(prop => (<Property key={prop.Description.Identifier} {...prop} />)) }
  </ToggleContent>
);

PropertyCollection.propTypes = {
  name: PropTypes.string.isRequired,
  properties: PropTypes.arrayOf(PropTypes.object),
};

PropertyCollection.defaultProps = {
  properties: [],
};

export default PropertyCollection;
