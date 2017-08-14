import React from 'react';
import PropTypes from 'prop-types';
import ToggleContent from '../../common/ToggleContent/ToggleContent';
import Property from './Property';
import BoolProperty from './BoolProperty';

const types = {
  BoolProperty,
  StringProperty: Property,
  defaultProp: Property,
};

const PropertyCollection = ({ name, properties }) => (
  <ToggleContent title={name}>
    { properties.map((prop) => {
      const { Description } = prop;
      const Type = types[Description.Type] || types.defaultProp;
      return (
        <Type key={Description.Identifier} {...prop} />
      );
    }) }
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
