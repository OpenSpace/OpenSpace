import React from 'react';
import PropTypes from 'prop-types';
import ReactSelect from 'react-select';
import 'react-select/dist/react-select.css';
import Input from '../../common/Input/Input/Input';
import Label from '../../common/Label/Label';
import Row from '../../common/Row/Row';
import styles from './MultiInputs.scss';

const MultiInputs = (props) => {
  const { options, onChange, label, subLabel } = props;
  return (
    <div className={styles.multiInputs}>
      <Label size='medium'>{label}:</Label>
        <Row className="hej">
          { Object.keys(options).map((key, index) => (
              <Input 
                key={key}
                label={key}
                placeholder={key}
                value={options[key]}
                onChange={onChange}
              />
          ))}
        </Row>
    </div>
  );
};

MultiInputs.propTypes = {
  options: PropTypes.object.isRequired,
  label: PropTypes.string,
  onChange: PropTypes.func,
}

MultiInputs.defaultProps = {
  options: {},
  label:'Loading',
  onChange: () => {},
}

export default MultiInputs;