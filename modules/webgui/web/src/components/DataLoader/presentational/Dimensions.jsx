import React from 'react';
import PropTypes from 'prop-types';
import ReactSelect from 'react-select';
import 'react-select/dist/react-select.css';
import Input from '../../common/Input/Input/Input';
import Label from '../../common/Label/Label';
import Row from '../../common/Row/Row';
import styles from './Dimensions.scss';
import { excludeKeys } from '../../../utils/helpers';

const Dimensions = (props) => {

  const { dimensions, onChange } = props;

  return (
    <Row className={styles.dimensions}>
      <Label size={'medium'}>Dimensions: </Label>
        <Row>
          { Object.keys(dimensions).map((key, index) => (
              <Input 
                key={key}
                label={key}
                placeholder={key}
                value={dimensions[key]}
                onChange={onChange}
              />
          ))}
        </Row>
    </Row>
  );
};

Dimensions.propTypes = {
  dimensions: PropTypes.object,
  onChange: PropTypes.func,
}

Dimensions.defaultProps = {
  dimensions: {
    x: 100,
    y: 100,
    z: 100,
  },
  onChange: () => {},
}

export default Dimensions;