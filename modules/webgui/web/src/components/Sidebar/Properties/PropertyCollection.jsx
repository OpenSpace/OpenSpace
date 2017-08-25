import React from 'react';
import PropTypes from 'prop-types';
import ToggleContent from '../../common/ToggleContent/ToggleContent';
import Property from './Property';
import BoolProperty from './BoolProperty';
import NumericProperty from './NumericProperty';
import OptionProperty from './OptionProperty';
import TriggerProperty from './TriggerProperty';

const types = {
  BoolProperty,
  OptionProperty,
  TriggerProperty,
  StringProperty: Property,
  NumericProperty,
  FloatProperty: NumericProperty,
  IntProperty: NumericProperty,
  defaultProperty: Property,
};

const PropertyCollection = ({ name, properties }) => (
  <ToggleContent title={name}>
    { properties.map((prop) => {
      const { Description } = prop;
      const Type = types[Description.Type] || types.defaultProperty;
      return (
        <Type key={Description.Identifier} {...prop} subscribe />
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
export const Types = types;
export const GetType = type => types[type] || types.defaultProperty;
